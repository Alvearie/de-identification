/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.seceng;

import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import org.junit.Test;

public class SecEngFileUtilsTest {
  private static final String TEST_DIR_NAME = "NonExistingDirName";
  private static final String TEST_FILE_NAME = "NonExistingFileName";

  @Test
  public void testGetFile() throws Exception {
    SecEngFileUtils obj = new SecEngFileUtils();
    assertNotNull(obj);

    File file = SecEngFileUtils.getFile(TEST_FILE_NAME);
    assertNotNull(file);

    file = SecEngFileUtils.getFile(TEST_FILE_NAME, SecEngFileUtils.FileOperation.Read);
    assertNotNull(file);

    file = SecEngFileUtils.getFile(TEST_DIR_NAME, TEST_FILE_NAME);
    assertNotNull(file);

    file =
        SecEngFileUtils.getFile(TEST_DIR_NAME, TEST_FILE_NAME, SecEngFileUtils.FileOperation.Read);
    assertNotNull(file);

    file = SecEngFileUtils.getFile(new File(TEST_DIR_NAME));
    assertNotNull(file);

    file = SecEngFileUtils.getFile(new File(TEST_DIR_NAME), SecEngFileUtils.FileOperation.Read);
    assertNotNull(file);

    file = SecEngFileUtils.getFile((File) null, TEST_FILE_NAME);
    assertNotNull(file);

    file = SecEngFileUtils.getFile((File) null, TEST_FILE_NAME, SecEngFileUtils.FileOperation.Read);
    assertNotNull(file);
  }

  @Test(expected = NullPointerException.class)
  public void testGetFile_NullDir() throws Exception {
    SecEngFileUtils.getFile((String) null, TEST_FILE_NAME);
  }

  @Test(expected = NullPointerException.class)
  public void testGetFile_NullFile() throws Exception {
    SecEngFileUtils.getFile((String) null);
  }

  @Test(expected = NullPointerException.class)
  public void testGetFile_NullFile2() throws Exception {
    SecEngFileUtils.getFile((File) null);
  }

  @Test(expected = NullPointerException.class)
  public void testGetFile_NullFileWithOperation() throws Exception {
    SecEngFileUtils.getFile((File) null, SecEngFileUtils.FileOperation.Read);
  }

  @Test
  public void testCreateTempFile() throws Exception {
    File file = SecEngFileUtils.createTempFile("temp", "");
    assertNotNull(file);

    file = SecEngFileUtils.createTempFile("temp", "", null);
    assertNotNull(file);

    try {
      file = SecEngFileUtils.createTempFile("temp", "", new File("."));
      assertNotNull(file);
    } catch (IOException e) {
      assertNotNull(file);
    }
  }

  @Test(expected = IOException.class)
  public void testCreateTempFile_ValidationError() throws Exception {
    SecEngFileUtils.createTempFile("../temp", "");
  }

  @Test(expected = NullPointerException.class)
  public void testValidatePath_Error() throws Exception {
    SecEngFileUtils.validatePath(null, null);
  }

  @Test(expected = NullPointerException.class)
  public void testValidatePath_Error2() throws Exception {
    SecEngFileUtils.validatePath(Paths.get("."), null);
  }
}
