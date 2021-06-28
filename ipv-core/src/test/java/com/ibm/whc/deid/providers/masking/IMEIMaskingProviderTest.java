/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.IMEIIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.IMEIMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;

public class IMEIMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  @Test
  public void testMask() {
    IMEIIdentifier identifier = new IMEIIdentifier(tenantId, localizationProperty);
    IMEIMaskingProviderConfig config = new IMEIMaskingProviderConfig();
    MaskingProvider maskingProvider =
        new IMEIMaskingProvider(config, tenantId, localizationProperty);

    String imei = "001013001234568";
    boolean changed = false;
    for (int i = 0; i < 10; i++) {
      // by default we preserve TAC
      String maskedValue = maskingProvider.mask(imei);
      assertEquals(imei.substring(0, 8), maskedValue.substring(0, 8));
      assertTrue(identifier.isOfThisType(maskedValue));
      if (!imei.equals(maskedValue)) {
        changed = true;
      }
    }
    assertTrue(changed);
  }

  @Test
  public void testMaskNoTACPreservation() {
    IMEIIdentifier identifier = new IMEIIdentifier(tenantId, localizationProperty);
    IMEIMaskingProviderConfig config = new IMEIMaskingProviderConfig();
    config.setPreserveTAC(false);
    MaskingProvider maskingProvider =
        new IMEIMaskingProvider(config, tenantId, localizationProperty);

    String imei = "001013001234568";
    boolean changedBody = false;
    boolean changedTAC = false;
    for (int i = 0; i < 10; i++) {
      String maskedValue = maskingProvider.mask(imei);
      if (!imei.substring(0, 8).equals(maskedValue.substring(0, 8))) {
        changedTAC = true;
      }
      if (!imei.substring(8).equals(maskedValue.substring(8))) {
        changedBody = true;
      }
      assertTrue(identifier.isOfThisType(maskedValue));
    }
    assertTrue(changedBody);
    assertTrue(changedTAC);
  }

  @Test
  public void testMaskInvalidValuePreserveTAC_null() {
    IMEIMaskingProviderConfig config = new IMEIMaskingProviderConfig();
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    MaskingProvider maskingProvider =
        new IMEIMaskingProvider(config, tenantId, localizationProperty);

    String imei = "foobar";
    assertNull(maskingProvider.mask(imei));
  }

  @Test
  public void testMaskInvalidValuePreserveTAC_random() {
    IMEIIdentifier identifier = new IMEIIdentifier(tenantId, localizationProperty);
    IMEIMaskingProviderConfig config = new IMEIMaskingProviderConfig();
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    MaskingProvider maskingProvider =
        new IMEIMaskingProvider(config, tenantId, localizationProperty);

    String imei = "foobar";
    String maskedValue = maskingProvider.mask(imei);
    assertTrue(identifier.isOfThisType(maskedValue));
  }

  @Test
  public void testMaskInvalidValueNoPreserveTAC() {
    IMEIIdentifier identifier = new IMEIIdentifier(tenantId, localizationProperty);
    IMEIMaskingProviderConfig config = new IMEIMaskingProviderConfig();
    config.setPreserveTAC(false);
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.ERROR_EXIT);
    MaskingProvider maskingProvider =
        new IMEIMaskingProvider(config, tenantId, localizationProperty);

    String imei = "foobar";
    String maskedValue = maskingProvider.mask(imei);
    assertTrue(identifier.isOfThisType(maskedValue));
  }
}
