/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.external.spark.udf;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.IOException;
import java.util.List;
import org.junit.Test;
import com.ibm.whc.deid.entry.api.DeidProcessor;
import com.ibm.whc.deid.external.spark.udf.DeIdUDF;
import com.ibm.whc.deid.shared.exception.KeyedIllegalArgumentException;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import com.ibm.whc.deid.utils.log.LogCodes;

public class DeIdUDFTest {

  @Test
  public void testMain() {

    class TestDeIdUDF1 extends DeIdUDF {
      private static final long serialVersionUID = 1L;

      @Override
      protected String getConfigString(String fileName) throws IOException {
        assertNotNull(fileName);
        String out = "";
        if ("/home/spark/shared/deid.masking.config.json".equals(fileName)) {
          out =
              "{\"rules\": [{\"name\": \"bin\", \"maskingProviders\": [{\"type\": \"BINNING\"}]}],\"json\": {\"schemaType\": \"GEN\", \"maskingRules\": [{\"jsonPath\": \"/gen/default/c\", \"rule\": \"bin\"}]}}";
        }
        return out;
      }

      @Override
      protected String getEnvVar(String name) {
        return null;
      }

      @Override
      protected DeidProcessor getProcessor(String masking)
          throws IOException, InvalidMaskingConfigurationException {
        return super.getProcessor(masking);
      }
    }

    TestDeIdUDF1 udf = new TestDeIdUDF1();
    assertEquals("{\"c\":\"5-10\"}", udf.call("{\"c\":6}"));
    assertNull(udf.call(null));
    try {
      udf.call("{invalid json");
      fail("expected exception");
    } catch (KeyedIllegalArgumentException e) {
      assertEquals(LogCodes.WPH1026E, e.getMessageKey());
      assertTrue(e.getMessage(), e.getMessage().contains("`0`"));
    }
  }

  @Test
  public void testBadConfig() {

    class TestDeIdUDF3 extends DeIdUDF {
      private static final long serialVersionUID = 1L;

      public String envVar;

      public TestDeIdUDF3(String envVar) {
        this.envVar = envVar;
      }

      @Override
      protected String getConfigString(String fileName) throws IOException {
        assertNotNull(fileName);
        String out = null;
        if ("/home/spark/shared/deid.masking.config.json".equals(fileName)) {
          out =
              "{\"rules\": [{\"name\" \"bin\", \"maskingProviders\": [{\"type\": \"BINNING\"}]}],\"json\": {\"schemaType\": \"GEN\", \"maskingRules\": [{\"jsonPath\": \"/gen/default/c\", \"rule\": \"bin\"}]}}";
        }
        return out;
      }

      @Override
      protected String getEnvVar(String name) {
        // should only be called for config path
        assertTrue(DeIdUDF.CONFIG_PATH_ENV_VAR.equals(name));
        return envVar;
      }
    }

    TestDeIdUDF3 udf = new TestDeIdUDF3(null);
    assertNull(udf.call(null));
    try {
      udf.call("{\"c\":6}");
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getCause().getClass().getName(), e.getCause() instanceof IOException);
    }

    // try whitespace instead of null for env variables
    udf = new TestDeIdUDF3("  ");
    assertNull(udf.call(null));
    try {
      udf.call("{\"c\":6}");
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getCause().getClass().getName(), e.getCause() instanceof IOException);
    }
  }

  @Test
  public void testGetDeidProcessor() {

    class TestDeIdUDF2 extends DeIdUDF {
      private static final long serialVersionUID = 1L;

      @Override
      protected String getConfigString(String fileName) throws IOException {
        String out = null;
        assertNotNull(fileName);
        switch (fileName) {
          case "/home/my-config/deid.masking.config.json":
            out = "masking";
            break;
          default:
            fail("unexpected file name " + fileName);
        }
        return out;
      }

      @Override
      protected String getEnvVar(String name) {
        String var = null;
        switch (name) {
          case "DEID_UDF_CONFIG_DIR":
            var = "/home/my-config";
            break;
          default:
            fail("unexpected variable name " + name);
        }
        return var;
      }

      @Override
      protected DeidProcessor getProcessor(String masking)
          throws IOException, InvalidMaskingConfigurationException {
        assertEquals("masking", masking);
        return new DeidProcessor() {

          @Override
          public List<String> process(List<String> input) {
            return null;
          }
        };
      }
    }

    TestDeIdUDF2 udf = new TestDeIdUDF2();
    DeidProcessor proc = udf.getDeidProcessor();
    assertNotNull(proc);
    // additional calls return cached value
    assertTrue(proc == udf.getDeidProcessor());
  }
}
