/*
 * © Merative US L.P. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.external.spark.udf;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.BufferedWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import org.junit.Test;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class DeIdSynapseUDFTest {

  private static class TestDeIdSynapseUDF extends DeIdSynapseUDF {
    private static final long serialVersionUID = 1L;

    public final HashMap<String, String> envVars = new HashMap<>();

    @Override
    protected String getEnvVar(String name) {
      return envVars.get(name);
    }

    @Override
    protected String getSynapseMountPoint() {
      return "/tmp";
    }
  }

  @Test
  public void testGetMaskingConfig_noEnvVars() throws Exception {

    TestDeIdSynapseUDF udf = new TestDeIdSynapseUDF();

    try {
      udf.getMaskingConfig();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("environment variable " + DeIdUDF.CONFIG_PATH_ENV_VAR + " is required",
          e.getMessage());
    }

    udf.envVars.put(DeIdUDF.CONFIG_PATH_ENV_VAR, "   ");

    try {
      udf.getMaskingConfig();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("environment variable " + DeIdUDF.CONFIG_PATH_ENV_VAR + " is required",
          e.getMessage());
    }

    udf.envVars.put(DeIdUDF.CONFIG_PATH_ENV_VAR, "\t");

    try {
      udf.getMaskingConfig();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("environment variable " + DeIdUDF.CONFIG_PATH_ENV_VAR + " is required",
          e.getMessage());
    }
  }

  @Test
  public void testGetMaskingConfig() throws Exception {
    TestDeIdSynapseUDF udf = new TestDeIdSynapseUDF();

    Path path1 = null;
    Path path1t = null;
    Path path2 = null;
    Path path2c = null;
    Path path2r = null;
    Path subpath = null;
    Path path1tfile = null;
    Path path2rfile = null;
    Path subpathfile = null;

    try {
      // no mounted job directories
      udf.envVars.put(DeIdUDF.CONFIG_PATH_ENV_VAR, "DeIdSynapseUDFTest4");
      try {
        udf.getMaskingConfig();
        fail("expected exception");
      } catch (InvalidMaskingConfigurationException e) {
        assertEquals("directory DeIdSynapseUDFTest4 not found within /tmp", e.getMessage());
      }
  
      // target not found
      path1 = Paths.get("/tmp", "1");
      Files.createDirectories(path1);
      try {
        udf.getMaskingConfig();
        fail("expected exception");
      } catch (InvalidMaskingConfigurationException e) {
        assertEquals("directory DeIdSynapseUDFTest4 not found within /tmp", e.getMessage());
      }

      // target found - no masking file environment variable
      path1t = Paths.get("/tmp", "1", "DeIdSynapseUDFTest4");
      Files.createDirectories(path1t);
      try {
        udf.getMaskingConfig();
        fail("expected exception");
      } catch (InvalidMaskingConfigurationException e) {
        assertEquals(
            "environment variable " + DeIdSynapseUDF.MASKING_CONFIG_FILE_ENV_VAR + " is required",
            e.getMessage());
      }
      udf.envVars.put(DeIdSynapseUDF.MASKING_CONFIG_FILE_ENV_VAR, "");
      try {
        udf.getMaskingConfig();
        fail("expected exception");
      } catch (InvalidMaskingConfigurationException e) {
        assertEquals(
            "environment variable " + DeIdSynapseUDF.MASKING_CONFIG_FILE_ENV_VAR + " is required",
            e.getMessage());
      }
      udf.envVars.put(DeIdSynapseUDF.MASKING_CONFIG_FILE_ENV_VAR, "   ");
      try {
        udf.getMaskingConfig();
        fail("expected exception");
      } catch (InvalidMaskingConfigurationException e) {
        assertEquals(
            "environment variable " + DeIdSynapseUDF.MASKING_CONFIG_FILE_ENV_VAR + " is required",
            e.getMessage());
      }

      // target found - no file
      udf.envVars.put(DeIdSynapseUDF.MASKING_CONFIG_FILE_ENV_VAR, DeIdUDF.MASKING_CONFIG_FILENAME);
      try {
        udf.getMaskingConfig();
        fail("expected exception");
      } catch (InvalidMaskingConfigurationException e) {
        assertEquals("masking configuration not found in /tmp", e.getMessage());
      }

      // target is subdirectory that is not found
      udf.envVars.put(DeIdUDF.CONFIG_PATH_ENV_VAR,
          "DeIdSynapseUDFTestCustomer/DeIdSynapseUDFTestRequest");
      try {
        udf.getMaskingConfig();
        fail("expected exception");
      } catch (InvalidMaskingConfigurationException e) {
        assertEquals(
            "directory DeIdSynapseUDFTestCustomer/DeIdSynapseUDFTestRequest not found within /tmp",
            e.getMessage());
      }

      path2 = Paths.get("/tmp", "2");
      path2c = Paths.get("/tmp", "2", "DeIdSynapseUDFTestCustomer");
      path2r = Paths.get("/tmp", "2", "DeIdSynapseUDFTestCustomer", "DeIdSynapseUDFTestRequest");
      subpath = Paths.get("/tmp", "2", "DeIdSynapseUDFTestCustomer", "DeIdSynapseUDFTestRequest",
          "DeIdSynapseUDFTest4");
      Files.createDirectories(subpath);
      path2rfile = Files.createFile(Paths.get(path2r.toString(), "masking.x.json"));
      try (BufferedWriter w = Files.newBufferedWriter(path2rfile)) {
        w.write("path2r content");
      }
      subpathfile =
          Files.createFile(Paths.get(subpath.toString(), "masking.x.json"));
      try (BufferedWriter w = Files.newBufferedWriter(subpathfile)) {
        w.write("subpath content");
      }
      udf.envVars.put(DeIdSynapseUDF.MASKING_CONFIG_FILE_ENV_VAR, "masking.x.json");

      // should return path2r, not subpath
      assertEquals("path2r content", udf.getMaskingConfig());

      // multiple paths contain a file - different content
      udf.envVars.put(DeIdUDF.CONFIG_PATH_ENV_VAR, "DeIdSynapseUDFTest4");
      path1tfile = Files.createFile(Paths.get(path1t.toString(), "masking.x.json"));
      try (BufferedWriter w = Files.newBufferedWriter(path1tfile)) {
        w.write("path1t content");
      }
      try {
        udf.getMaskingConfig();
        fail("expected exception");
      } catch (InvalidMaskingConfigurationException e) {
        System.out.println(e.getMessage());
        assertTrue(e.getMessage().startsWith("multiple possible config directories found: ["));
        assertTrue(e.getMessage().contains(path1t.toString()));
        assertTrue(e.getMessage().contains(subpath.toString()));
      }

      // multiple paths contain a file - same content
      try (BufferedWriter w = Files.newBufferedWriter(subpathfile)) {
        w.write("path1t content");
      }
      assertEquals("path1t content", udf.getMaskingConfig());

    } finally {
      delete(path1tfile);
      delete(path2rfile);
      delete(subpathfile);
      delete(path1t);
      delete(path1);
      delete(subpath);
      delete(path2r);
      delete(path2c);
      delete(path2);
    }
  }

  private void delete(Path path) {
    if (path != null) {
      try {
        Files.delete(path);
      } catch (Exception e) {
        e.printStackTrace();
      }
    }
  }

  @Test
  public void testMountPoint() {
    DeIdSynapseUDF udf = new DeIdSynapseUDF();
    assertEquals("/synfs", udf.getSynapseMountPoint());
  }
}
