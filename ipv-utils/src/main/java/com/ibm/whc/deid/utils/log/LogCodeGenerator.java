/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.utils.log;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Properties;
import java.util.TreeSet;

public class LogCodeGenerator {

  protected static final String LOG_CODES_JAVA =
      File.separator + "src" + File.separator + "main" + File.separator + "java" + File.separator
          + "com" + File.separator + "ibm" + File.separator + "whc" + File.separator + "deid"
          + File.separator + "utils" + File.separator + "log" + File.separator + "LogCodes.java";

  protected static final String LOG_CODES = File.separator + "src" + File.separator + "main"
      + File.separator + "resources" + File.separator + "logCodes.properties";

  protected static String WORK_DIR = System.getProperty("user.dir");

  private static final LogManager logger = LogManager.getInstance();

  public static void main(String[] args) throws IOException {

    getFileBasePath();
    Properties properties = new Properties() {
      private static final long serialVersionUID = -2144747756976051516L;

      @Override
      public synchronized Object put(Object key, Object value) {
        if (get(key) != null) {
          throw new IllegalArgumentException(
              key + " already present." + "Duplicated key is not allowed in logCodes.properties");
        }
        return super.put(key, value);
      }
    };

    try (InputStream inputStream = new FileInputStream(WORK_DIR + LOG_CODES)) {
      properties.load(inputStream);
    }

    Path logCodes_class_file = Paths.get(WORK_DIR + LOG_CODES_JAVA);
    File logCodesJava = logCodes_class_file.toFile();

    if (logCodesJava.exists()) {
      if (logCodesJava.delete() == Boolean.FALSE) {
        throw new IOException("Deletion of the old logCodes.properties file failed");
      }
    }

    Files.createFile(logCodes_class_file);
    generate(properties, logCodesJava.getAbsolutePath());
  }

  /** IDE dir is different from the maven build Therefore, pre process the file path */
  private static void getFileBasePath() {

    if (!WORK_DIR.contains("ipv-utils")) {
      WORK_DIR = WORK_DIR + File.separator + "ipv-utils";
    }
  }

  /** Generates log error codes for de-id */
  public static void generate(Properties properties, String filePath) {
    try (FileWriter aWriter = new FileWriter(filePath, true)) {
      aWriter.write("/*\n" + " * (C) Copyright IBM Corp. 2016,2021\n" + " *\n"
          + " * SPDX-License-Identifier: Apache-2.0\n" + " */\n\n");

      aWriter.write("package com.ibm.whc.deid.utils.log;\n\n");

      aWriter.write("/**\n"
          + " * Generated code. DO NOT edit manually. To add additional log codes, add them to\n"
          + " * \n"
          + " * <a href= \"file:../resources/logCodes.properties\">/resources/logCodes.properties</a>\n"
          + " * \n" + " * and run mvn compile\n" + " *\n" + " */\n");

      aWriter.write("public interface LogCodes {\n");

      TreeSet<String> sortedKeys = new TreeSet<String>(properties.stringPropertyNames());
      sortedKeys.forEach(key -> {
        try {
          aWriter
              .write("  public static final String " + key + " = \"" + key + "\";\n");
        } catch (IOException e1) {
          logger.logError(LogCodes.WPH4000E, e1);
        }
      });
      aWriter.write("}\n");
      aWriter.flush();
      aWriter.close();
    } catch (IOException ex) {
      logger.logError(LogCodes.WPH4000E, ex);
    }
  }
}
