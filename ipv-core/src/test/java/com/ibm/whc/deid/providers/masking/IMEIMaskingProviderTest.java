/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.providers.identifiers.IMEIIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.IMEIMaskingProviderConfig;
import org.junit.Test;

public class IMEIMaskingProviderTest extends TestLogSetUp {
  @Test
  public void testMask() {
    IMEIIdentifier identifier = new IMEIIdentifier();
    IMEIMaskingProviderConfig config = new IMEIMaskingProviderConfig();
    MaskingProvider maskingProvider = new IMEIMaskingProvider(config);

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
    config.setPreserveTac(Boolean.FALSE);
    MaskingProvider maskingProvider = new IMEIMaskingProvider(config);

    IMEIIdentifier identifier = new IMEIIdentifier();

    String imei = "001013001234568";
    String maskedValue = maskingProvider.mask(imei);

    assertFalse(maskedValue.substring(0, 8).equals(imei.substring(0, 8)));
    assertTrue(identifier.isOfThisType(maskedValue));
    assertFalse(maskedValue.equals(imei));
  }

  @Test
  public void testMaskInvalidValue() {
    IMEIMaskingProviderConfig config = new IMEIMaskingProviderConfig();
    MaskingProvider maskingProvider = new IMEIMaskingProvider(config);

    String imei = "foobar";
    String maskedValue = maskingProvider.mask(imei);

    assertEquals(null, maskedValue);
  }
}
