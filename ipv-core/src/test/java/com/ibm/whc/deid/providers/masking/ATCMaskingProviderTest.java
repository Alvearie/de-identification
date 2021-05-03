/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.ATCMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class ATCMaskingProviderTest implements MaskingProviderTest {

  /*
   * Tests for mask levelsToKeep option for all 5 level values (1 through 5).
   * It also tests for an invalid value.
   */

  @Test
  public void testMask() {
    ATCMaskingProviderConfig configuration = new ATCMaskingProviderConfig();
    configuration.setMaskLevelsToKeep(1);
    MaskingProvider maskingProvider = new ATCMaskingProvider(configuration, tenantId, localizationProperty);

    String atc = "A04AA02";
    String maskedValue = maskingProvider.mask(atc);
    assertTrue(maskedValue.equals("A"));

    configuration.setMaskLevelsToKeep(2);
    maskingProvider = new ATCMaskingProvider(configuration, tenantId, localizationProperty);
    maskedValue = maskingProvider.mask(atc);
    assertTrue(maskedValue.equals("A04"));

    configuration.setMaskLevelsToKeep(3);
    maskingProvider = new ATCMaskingProvider(configuration, tenantId, localizationProperty);
    maskedValue = maskingProvider.mask(atc);
    assertTrue(maskedValue.equals("A04A"));

    configuration.setMaskLevelsToKeep(4);
    maskingProvider = new ATCMaskingProvider(configuration, tenantId, localizationProperty);
    maskedValue = maskingProvider.mask(atc);
    assertTrue(maskedValue.equals("A04AA"));

    configuration.setMaskLevelsToKeep(5);
    try {
      maskingProvider = new ATCMaskingProvider(configuration, tenantId, localizationProperty);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getCause() instanceof InvalidMaskingConfigurationException);
      assertTrue(e.getCause().getMessage().contains("`maskLevelsToKeep`"));
    }
  }

  @Test
  public void testMaskInvalidValueValidHandlingReturnNull() {
    ATCMaskingProviderConfig configuration = new ATCMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    MaskingProvider maskingProvider = new ATCMaskingProvider(configuration, tenantId, localizationProperty);

    String atc = "foobar";
    String maskedValue = maskingProvider.mask(atc);
    assertNull(maskedValue);
  }

  @Test
  public void testMaskInvalidValueValidHandlingReturnRandom() {
    ATCMaskingProviderConfig configuration = new ATCMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    MaskingProvider maskingProvider = new ATCMaskingProvider(configuration, tenantId, localizationProperty);

    // return NULL, no random generation
    String atc = "foobar";
    String maskedValue = maskingProvider.mask(atc);
    assertNull(maskedValue);
  }
}
