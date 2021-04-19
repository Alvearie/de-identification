/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.util.MaritalStatusManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class MaritalStatusManagerTest {

  String tenantId = "TEST_TENANT";
  private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

  @Test
  public void testLookup() throws Exception {
    MaritalStatusManager maritalStatusManager =
        MaritalStatusManager.buildMaritalStatusManager(localizationProperty);
    String status = "Single";
    assertTrue(maritalStatusManager.isValidKey(status));

    status = "singLE";
    assertTrue(maritalStatusManager.isValidKey(status));

    status = "xxx";
    assertFalse(maritalStatusManager.isValidKey(status));
  }

  @Test
  public void testRandomCodeGenerator() throws Exception {
    MaritalStatusManager maritalStatusManager =
        MaritalStatusManager.buildMaritalStatusManager(localizationProperty);
    assertTrue(maritalStatusManager.isValidKey(maritalStatusManager.getRandomKey()));
  }
}
