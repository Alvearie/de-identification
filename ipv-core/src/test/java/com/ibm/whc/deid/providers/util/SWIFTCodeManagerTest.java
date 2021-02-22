/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.models.SWIFTCode;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.util.SWIFTCodeManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class SWIFTCodeManagerTest implements MaskingProviderTest {

	private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

  @Test
  public void testLookup() {
    SWIFTCodeManager swiftCodeManager = new SWIFTCodeManager(tenantId, localizationProperty);

    String key = "EMCRGRA1";
    assertTrue(swiftCodeManager.isValidKey(key));

    SWIFTCode code = swiftCodeManager.getKey(key);
    assertTrue(code.getCode().equals(key));
    assertTrue(code.getCountry().getName().toUpperCase().equals("GREECE"));
  }

  @Test
  public void testCodeFromCountry() {
    SWIFTCodeManager swiftCodeManager = new SWIFTCodeManager(tenantId, localizationProperty);

    String validCode = "EMCRGRA1";
    assertTrue(swiftCodeManager.isValidKey(validCode));
    assertNotNull(swiftCodeManager.getCodeFromCountry(validCode));

    String invalidCode = "INVALID_CODE";
    assertFalse(swiftCodeManager.isValidKey(invalidCode));
    assertNotNull(swiftCodeManager.getCodeFromCountry(invalidCode));
  }
}
