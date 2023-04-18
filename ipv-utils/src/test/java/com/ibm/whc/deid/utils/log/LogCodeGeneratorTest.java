/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.utils.log;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Properties;
import org.junit.After;
import org.junit.Test;

public class LogCodeGeneratorTest extends LogCodeGenerator {

  @After
  public void teardown() {
    File file = new File("testFilePath");
    if (file.exists())
      file.delete();
  }

  @Test
  public void testMain() throws IOException {
    String[] args = {};
    LogCodeGenerator.main(args);
    try (FileReader aReader = new FileReader(WORK_DIR + LOG_CODES_JAVA)) {
      int i = aReader.read();
      assertNotNull((char) i);
    } catch (IOException e) {
      fail();
    }
  }

  @Test
  public void testGenerate() {
    Properties properties = new Properties();
    String filepath = "testFilePath";
    LogCodeGenerator.generate(properties, filepath);
    try (FileReader aReader = new FileReader(filepath)) {
      int i = aReader.read();
      assertNotNull((char) i);
    } catch (IOException e) {
      fail();
    }
  }

}
