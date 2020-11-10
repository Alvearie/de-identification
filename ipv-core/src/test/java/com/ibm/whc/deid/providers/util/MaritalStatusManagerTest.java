/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.util.MaritalStatusManager;
import org.junit.Test;

public class MaritalStatusManagerTest {

  String tenantId = "TEST_TENANT";

  @Test
  public void testLookupSuccessful() throws Exception {
    MaritalStatusManager maritalStatusManager = new MaritalStatusManager(tenantId);
    String status = "Single";
    assertTrue(maritalStatusManager.isValidKey(status));

    status = "singLE";
    assertTrue(maritalStatusManager.isValidKey(status));
  }

  @Test
  public void testRandomCodeGenerator() throws Exception {
    MaritalStatusManager maritalStatusManager = new MaritalStatusManager(tenantId);
    assertTrue(maritalStatusManager.isValidKey(maritalStatusManager.getRandomKey()));
  }
}
