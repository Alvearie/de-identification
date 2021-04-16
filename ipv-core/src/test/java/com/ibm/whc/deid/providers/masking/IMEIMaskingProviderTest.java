/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.providers.identifiers.IMEIIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.IMEIMaskingProviderConfig;

public class IMEIMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  @Test
  public void testMask() {
    IMEIIdentifier identifier = new IMEIIdentifier(tenantId, localizationProperty);
    IMEIMaskingProviderConfig config = new IMEIMaskingProviderConfig();
    MaskingProvider maskingProvider =
        new IMEIMaskingProvider(config, tenantId, localizationProperty);

    String imei = "001013001234568";
    String maskedValue = maskingProvider.mask(imei);

    // by default we preserve TAC
    assertTrue(maskedValue.substring(0, 8).equals(imei.substring(0, 8)));
    assertTrue(identifier.isOfThisType(maskedValue));
    assertFalse(maskedValue.equals(imei));
  }

  @Test
  public void testMaskNoTACPreservation() {
    IMEIMaskingProviderConfig config = new IMEIMaskingProviderConfig();
    config.setPreserveTAC(false);
    MaskingProvider maskingProvider =
        new IMEIMaskingProvider(config, tenantId, localizationProperty);

    IMEIIdentifier identifier = new IMEIIdentifier(tenantId, localizationProperty);

    String imei = "001013001234568";
    String maskedValue = maskingProvider.mask(imei);

    assertFalse(maskedValue.substring(0, 8).equals(imei.substring(0, 8)));
    assertTrue(identifier.isOfThisType(maskedValue));
    assertFalse(maskedValue.equals(imei));
  }

  @Test
  public void testMaskInvalidValue() {
    IMEIMaskingProviderConfig config = new IMEIMaskingProviderConfig();
    MaskingProvider maskingProvider =
        new IMEIMaskingProvider(config, tenantId, localizationProperty);

    String imei = "foobar";
    String maskedValue = maskingProvider.mask(imei);

    assertEquals(null, maskedValue);
  }
}
