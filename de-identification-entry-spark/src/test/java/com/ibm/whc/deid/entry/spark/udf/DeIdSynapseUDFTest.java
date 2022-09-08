/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.entry.spark.udf;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
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
  public void testGetConfigDirectory_noEnvVars() throws Exception {

    TestDeIdSynapseUDF udf = new TestDeIdSynapseUDF();

    try {
      udf.getConfigDirectory();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("environment variable " + DeIdUDF.CONFIG_PATH_ENV_VAR + " is required",
          e.getMessage());
    }

    udf.envVars.put(DeIdUDF.CONFIG_PATH_ENV_VAR, "   ");

    try {
      udf.getConfigDirectory();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("environment variable " + DeIdUDF.CONFIG_PATH_ENV_VAR + " is required",
          e.getMessage());
    }

    udf.envVars.put(DeIdUDF.CONFIG_PATH_ENV_VAR, "\t");

    try {
      udf.getConfigDirectory();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("environment variable " + DeIdUDF.CONFIG_PATH_ENV_VAR + " is required",
          e.getMessage());
    }
  }

  @Test
  public void testGetConfigDirectory() throws Exception {
    TestDeIdSynapseUDF udf = new TestDeIdSynapseUDF();

    Path path1 = null;
    Path path1t = null;
    Path path2 = null;
    Path path2c = null;
    Path path2r = null;
    Path subpath = null;

    try {
      // no mounted job directories
      udf.envVars.put(DeIdUDF.CONFIG_PATH_ENV_VAR, "DeIdSynapseUDFTest4");
      try {
        udf.getConfigDirectory();
        fail("expected exception");
      } catch (InvalidMaskingConfigurationException e) {
        assertEquals("directory DeIdSynapseUDFTest4 not found within /tmp", e.getMessage());
      }
  
      // target not found
      path1 = Paths.get("/tmp", "1");
      Files.createDirectories(path1);
      try {
        udf.getConfigDirectory();
        fail("expected exception");
      } catch (InvalidMaskingConfigurationException e) {
        assertEquals("directory DeIdSynapseUDFTest4 not found within /tmp", e.getMessage());
      }

      // target found
      path1t = Paths.get("/tmp", "1", "DeIdSynapseUDFTest4");
      Files.createDirectories(path1t);
      assertEquals(path1t.toString(), udf.getConfigDirectory());

      // target in subdirectory - not found
      udf.envVars.put(DeIdUDF.CONFIG_PATH_ENV_VAR,
          "DeIdSynapseUDFTestCustomer/DeIdSynapseUDFTestRequest");
      try {
        udf.getConfigDirectory();
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
      // should return path2r, not subpath
      assertEquals(path2r.toString(), udf.getConfigDirectory());

      udf.envVars.put(DeIdUDF.CONFIG_PATH_ENV_VAR, "DeIdSynapseUDFTest4");
      try {
        udf.getConfigDirectory();
        fail("expected exception");
      } catch (InvalidMaskingConfigurationException e) {
        System.out.println(e.getMessage());
        assertTrue(e.getMessage().startsWith("multiple possible config directories found: ["));
        assertTrue(e.getMessage().contains(path1t.toString()));
        assertTrue(e.getMessage().contains(subpath.toString()));
      }

    } finally {
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
}
