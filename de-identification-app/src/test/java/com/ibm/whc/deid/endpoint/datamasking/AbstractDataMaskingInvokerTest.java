/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.endpoint.datamasking;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.exception.InvalidInputException;

public class AbstractDataMaskingInvokerTest {

  private class TestAbstractDataMaskingInvoker extends AbstractDataMaskingInvoker {
    // just need concrete class
  }

  @Test
  public void testValidateGlobalConfig() throws Exception {

    TestAbstractDataMaskingInvoker app = new TestAbstractDataMaskingInvoker();

    app.validateGlobalConfig(null);
    app.validateGlobalConfig("    ");

    try {
      app.validateGlobalConfig("{    ");
      fail("expected exception");
    } catch (InvalidInputException e) {
      assertTrue(e.getMessage().contains("could not parse global configuration: "));
    }

    try {
      app.validateGlobalConfig("[4,3,4]");
      fail("expected exception");
    } catch (InvalidInputException e) {
      System.out.println(e.getMessage());
      assertTrue(e.getMessage().contains("could not parse global configuration: "));
    }

    app.validateGlobalConfig("{\"ruleSet\": null}");
    app.validateGlobalConfig("{\"ruleSet\": \"\"}");

    try {
      app.validateGlobalConfig("{\"ruleSet\": \" \"}");
      fail("expected exception");
    } catch (InvalidInputException e) {
      assertTrue(e.getMessage().contains("illegal value in global configuration"));
    }

    app.validateGlobalConfig("{\"ruleSet\": \"default\"}");
    
    try {
      app.validateGlobalConfig("{\"ruleSet\": \"Default\"}");
      fail("expected exception");
    } catch (InvalidInputException e) {
      assertTrue(e.getMessage().contains("illegal value in global configuration"));
    }
  }
}
