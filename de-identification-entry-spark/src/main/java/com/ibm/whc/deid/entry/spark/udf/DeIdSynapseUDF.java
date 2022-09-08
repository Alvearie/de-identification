/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.entry.spark.udf;

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
 * the UDF is called. Configuration is only read once. For security purposes, the database password
 * and NLP provider API key can be supplied by environment variables. If the environment variable is
 * found and has a value, its value replaces the value in the configuration file.
 */
// Including "implements" of an inherited interface is redundant,
// but required for pyspark in Azure Synapse.
public class DeIdSynapseUDF extends DeIdUDF implements UDF1<String, String> {

  private static final long serialVersionUID = 8991156671414592197L;

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
   * @return The absolute path name of the directory that should contain the configuration
   *         information for the De-ID UDF.
   */
  // FileSystem will not be closed
  @SuppressWarnings("resource")
  @Override
  protected String getConfigDirectory() throws IOException, InvalidMaskingConfigurationException {
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
    if (targets.size() > 1) {
      throw new InvalidMaskingConfigurationException(
          "multiple possible config directories found: " + targets.toString());
    }
    return targets.get(0).toString();
  }

  protected String getSynapseMountPoint() {
    return AZURE_SYNAPSE_STORAGE_ACCOUNT_MOUNT_POINT;
  }
}
