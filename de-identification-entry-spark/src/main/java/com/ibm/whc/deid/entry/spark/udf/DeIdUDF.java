/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.entry.spark.udf;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import org.apache.spark.sql.api.java.UDF1;
import com.ibm.whc.deid.entry.api.DeidEntry;
import com.ibm.whc.deid.entry.api.DeidProcessor;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/**
 * A Spark UDF implementation that accepts one string containing a JSON document and returns a
 * string containing the given JSON document with configured field values replaced with values from
 * the configured privacy providers.
 * 
 * <p>
 * The configuration of the De-Identification operation is read from the file system the first time
 * the UDF is called. Configuration is only read once.
 */
public class DeIdUDF implements UDF1<String, String> {

  private static final long serialVersionUID = 8991156671414592197L;

  /**
   * Environment variable that contains the path to the directory where the configuration files
   * reside. If not provided or whitespace, a default value is used.
   */
  public static final String CONFIG_PATH_ENV_VAR = "DEID_UDF_CONFIG_DIR";

  /**
   * Default path to the directory containing the configuration files.
   */
  public static final String DEFAULT_CONFIG_PATH = "/home/spark/shared/";

  /**
   * Name of the file containing the masking configuration in JSON format. This file is required.
   */
  public static final String MASKING_CONFIG_FILENAME = "deid.masking.config.json";

  protected transient volatile DeidProcessor processor = null;

  /**
   * Spark single input UTF interface method.
   */
  @Override
  public String call(String jsonDocument) {
    String maskedDocument = null;
    if (jsonDocument != null) {
      ArrayList<String> in = new ArrayList<>(1);
      in.add(jsonDocument);
      List<String> out = getDeidProcessor().process(in);
      maskedDocument = out.get(0);
    }
    return maskedDocument;
  }

  /**
   * One-time initialization of the De-Identification processor
   * 
   * @return the processor
   */
  protected DeidProcessor getDeidProcessor() {
    if (processor == null) {
      try {
        String configPath = getConfigDirectory();
        if (!configPath.endsWith("/")) {
          configPath += "/";
        }

        String maskingPath = configPath + MASKING_CONFIG_FILENAME;
        String config = getConfigString(maskingPath);
        if (config == null) {
          throw new InvalidMaskingConfigurationException("masking configuration not found at " + maskingPath);
        }

        processor = getProcessor(config);

      } catch (IOException | InvalidMaskingConfigurationException e) {
        throw new RuntimeException(e);
      }
    }
    return processor;
  }

  protected String getConfigString(String fileName) throws IOException {
    String out = null;
    File file = new File(fileName);
    if (file.exists()) {
      out = new String(Files.readAllBytes(file.toPath()), StandardCharsets.UTF_8);
    }
    return out;
  }

  protected String getEnvVar(String name) {
    return System.getenv(name);
  }

  /**
   * Finds the name of the directory in the local filesystem that contains the UDF configuration
   * files.
   * 
   * @return the name of the directory
   * 
   * @throws IOException if an error occurs accessing the filesystem
   * @throws InvalidMaskingConfigurationException if the provided config directory specification is
   *         in some way invalid
   */
  protected String getConfigDirectory() throws IOException, InvalidMaskingConfigurationException {
    String configuredConfigPath = getEnvVar(CONFIG_PATH_ENV_VAR);
    return configuredConfigPath == null || configuredConfigPath.trim().isEmpty()
        ? DEFAULT_CONFIG_PATH
        : configuredConfigPath;
  }

  protected DeidProcessor getProcessor(String masking)
      throws IOException, InvalidMaskingConfigurationException {
    return DeidEntry.getDeidProcessor(masking);
  }
}
