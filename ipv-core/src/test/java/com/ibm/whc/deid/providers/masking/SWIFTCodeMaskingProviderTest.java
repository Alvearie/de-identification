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
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.SWIFTCodeIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.SWIFTMaskingProviderConfig;

public class SWIFTCodeMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  @Test
  public void testPreserveCountry() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    maskingConfiguration.setPreserveCountry(true);
    MaskingProvider maskingProvider = new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId);
    Identifier identifier = new SWIFTCodeIdentifier();

    String key = "EMCRGRA1";
    String countryCodeOriginal = key.substring(4, 6);
    String maskedValue = maskingProvider.mask(key);
    String countryCodeMasked = maskedValue.substring(4, 6);

    System.out.println(maskedValue);
    assertTrue(identifier.isOfThisType(maskedValue));
    assertEquals(countryCodeOriginal, countryCodeMasked);
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

  @Test
  public void testInvalidInput() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    MaskingProvider maskingProvider = new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId);
    Identifier identifier = new SWIFTCodeIdentifier();

    assertNull(maskingProvider.mask((String) null));
    assertNull(maskingProvider.mask(""));
    assertNull(maskingProvider.mask(" "));
    assertNull(maskingProvider.mask("invalid"));

    maskingConfiguration.setUnspecifiedValueHandling(2);
    maskingProvider = new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId);

    assertNull(maskingProvider.mask((String) null));
    String maskedValue = maskingProvider.mask("");
    assertTrue(identifier.isOfThisType(maskedValue));
    maskedValue = maskingProvider.mask(" ");
    assertTrue(identifier.isOfThisType(maskedValue));
    maskedValue = maskingProvider.mask("invalid");
    assertTrue(identifier.isOfThisType(maskedValue));

    maskingConfiguration.setUnspecifiedValueHandling(3);
    maskingProvider = new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId);

    assertNull(maskingProvider.mask((String) null));
    assertEquals("OTHER", maskingProvider.mask(""));
    assertEquals("OTHER", maskingProvider.mask(" "));
    assertEquals("OTHER", maskingProvider.mask("invalid"));

    maskingConfiguration.setUnspecifiedValueHandling(1);
    maskingProvider = new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId);

    assertNull(maskingProvider.mask((String) null));
    assertNull(maskingProvider.mask(""));
    assertNull(maskingProvider.mask(" "));
    assertNull(maskingProvider.mask("invalid"));

    maskingConfiguration.setUnspecifiedValueHandling(3);
    maskingConfiguration.setUnspecifiedValueReturnMessage("msg");
    maskingProvider = new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId);

    assertNull(maskingProvider.mask((String) null));
    assertEquals("msg", maskingProvider.mask(""));
    assertEquals("msg", maskingProvider.mask(" "));
    assertEquals("msg", maskingProvider.mask("invalid"));
  }
}
