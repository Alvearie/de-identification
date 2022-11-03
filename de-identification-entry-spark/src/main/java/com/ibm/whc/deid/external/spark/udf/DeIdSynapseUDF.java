/*
 * © Merative US L.P. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.external.spark.udf;

import java.io.IOException;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.apache.spark.sql.api.java.UDF1;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/**
 * Subclass of the DeIdUDF class that obtains its configuration from a mounted filesystem specific
 * to the Azure Synapse pyspark environment.
 * 
 * <p>
 * The configuration of the de-identification operation is read from the file system the first time
 * the UDF is called. Configuration is only read once.
 */
// Including "implements" of an inherited interface is redundant,
// but required for pyspark in Azure Synapse.
public class DeIdSynapseUDF extends DeIdUDF implements UDF1<String, String> {

  private static final long serialVersionUID = 8991156671414592197L;

  /**
   * Environment variable that contains the name of the masking configuration file. This value is
   * required.
   */
  public static final String MASKING_CONFIG_FILE_ENV_VAR = "DEID_UDF_MASKING_CONFIG_FILE";

  protected static final String AZURE_SYNAPSE_STORAGE_ACCOUNT_MOUNT_POINT = "/synfs";
  protected static final Pattern jobIdPattern = Pattern.compile("\\d+");

  /**
   * Searches for the directory identified in the CONFIG_PATH_ENV_VAR environment variable. It is
   * assumed the value represents a path to a directory in an Azure Data Lake Gen2 Storage Account
   * filesystem that is mounted on each node in the Azure Synapse Spark Pool where this User-Defined
   * Function (UDF) will be executed.
   * 
   * Storage Account filesystems are always mounted on the Spark nodes under the directory
   * /synfs/<job-id>, where <job-id> is the current notebook or pipeline job number in Synapse. This
   * value is not known to the UDF, so the UDF will search for the target directory in all
   * directories in the filesystem that start with this path.
   * 
   * After /synfs/<job-id>, the next component of the path is the mount point name given when the
   * filesystem was mounted. Finally, the rest of the path is the path to the target directory
   * within the Storage Account filesystem itself.
   * 
   * If multiple masking configurations are found because multiple mounted file systems are found to
   * contain the target path and file name an exception is thrown unless the content of all such
   * files is exactly the same. In that case, the system continues using that configuration data.
   * 
   * @return the masking configuration as a string in JSON format
   * 
   * @throws InvalidMaskingConfigurationException if a required environment variable does not have a
   *         valid value, if the masking configuration cannot be found, or multiple, different
   *         masking configurations are found.
   */
  // FileSystem will not be closed
  @SuppressWarnings("resource")
  @Override
  protected String getMaskingConfig() throws IOException, InvalidMaskingConfigurationException {
    // do not call fileSystem.close() - not strictly required and supported on all operating systems
    FileSystem fileSystem = FileSystems.getDefault();

    String targetDir = getEnvVar(CONFIG_PATH_ENV_VAR);
    // this environment variable is required in this subclass even though it is
    // optional in the superclass
    if (targetDir == null || targetDir.trim().isEmpty()) {
      throw new InvalidMaskingConfigurationException(
          "environment variable " + CONFIG_PATH_ENV_VAR + " is required");
    }
    // make relative path
    if (targetDir.charAt(0) == '/' || targetDir.charAt(0) == '\\') {
      targetDir = targetDir.substring(1);
      if (targetDir.trim().isEmpty()) {
        throw new InvalidMaskingConfigurationException(
            "environment variable " + CONFIG_PATH_ENV_VAR + " must not be the root directory");
      }
    }
    Path targetDirPath = fileSystem.getPath(targetDir);

    Path path = fileSystem.getPath(getSynapseMountPoint());
    List<Path> pathsToCheck = Files.list(path)
        .filter(child -> jobIdPattern.matcher(child.getFileName().toString()).matches())
        .collect(Collectors.toList());

    List<Path> targets = new ArrayList<>();
    for (Path pathToCheck : pathsToCheck) {
      targets.addAll(Files
          .find(pathToCheck, 40,
              (child, attrs) -> {
                return child.endsWith(targetDirPath) && attrs.isDirectory();
              })
          .collect(Collectors.toList()));
    }

    if (targets.isEmpty()) {
      throw new InvalidMaskingConfigurationException(
          "directory " + targetDir + " not found within " + getSynapseMountPoint());
    }
    
    String maskingConfigFileName = getEnvVar(MASKING_CONFIG_FILE_ENV_VAR);
    if (maskingConfigFileName == null || maskingConfigFileName.trim().isEmpty()) {
      throw new InvalidMaskingConfigurationException(
          "environment variable " + MASKING_CONFIG_FILE_ENV_VAR + " is required");
    }

    String maskingConfig = null;
    for (Path targetPath : targets) {
      String configPath = targetPath.toString();
      if (!configPath.endsWith("/")) {
        configPath += "/";
      }
      String maskingPath = configPath + maskingConfigFileName;

      String config = getFileContentAsString(maskingPath);
      
      if (config != null) {
        if (maskingConfig == null) {
          maskingConfig = config;
        } else if (!maskingConfig.equals(config)) {
            throw new InvalidMaskingConfigurationException(
                "multiple possible config directories found: " + targets.toString());
        }
      }
    }

    if (maskingConfig == null) {
      throw new InvalidMaskingConfigurationException(
          "masking configuration not found in " + getSynapseMountPoint());
    }
    return maskingConfig;
  }

  protected String getSynapseMountPoint() {
    return AZURE_SYNAPSE_STORAGE_ACCOUNT_MOUNT_POINT;
  }
}
