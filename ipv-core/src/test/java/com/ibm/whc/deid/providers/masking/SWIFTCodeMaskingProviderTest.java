/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.SWIFTCodeIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.SWIFTMaskingProviderConfig;

public class SWIFTCodeMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  @Test
  public void testDefault() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    MaskingProvider maskingProvider = new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId);
    Identifier identifier = new SWIFTCodeIdentifier();

    String key = "EMCRGRA1";
    String maskedValue = maskingProvider.mask(key);
    assertTrue(identifier.isOfThisType(maskedValue));
  }

  @Test
  public void testPreserveCountry() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    maskingConfiguration.setPreserveCountry(Boolean.TRUE);
    MaskingProvider maskingProvider = new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId);
    Identifier identifier = new SWIFTCodeIdentifier();

    String key = "EMCRGRA1";
    String countryCodeOriginal = key.substring(4, 6);
    String maskedValue = maskingProvider.mask(key);
    String countryCodeMasked = maskedValue.substring(4, 6);

    System.out.println(maskedValue);
    assertTrue(identifier.isOfThisType(maskedValue));
    assertTrue(countryCodeOriginal.equals(countryCodeMasked));
  }

  @Test
  public void testMask() {
    Identifier swiftIdentifier = new SWIFTCodeIdentifier();
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    MaskingProvider maskingProvider = new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId);

    String value = "EMCRGRA1";
    int randomizationOK = 0;

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      assertTrue(swiftIdentifier.isOfThisType(maskedValue));

      if (!maskedValue.equals(value)) {
        randomizationOK++;
      }
    }

    assertTrue(randomizationOK > 0);
  }

}
