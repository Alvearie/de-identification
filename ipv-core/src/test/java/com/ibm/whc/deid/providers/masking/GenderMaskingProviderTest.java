/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.GenderMaskingProviderConfig;

public class GenderMaskingProviderTest implements MaskingProviderTest {

  /*
   * Note: Masking returns a random distribution of masked Male and Female values.
   * GenderMaskingProvider does not check the original value for a valid gender value (male, female,
   * F, M, man/men woman/women, both) and for any original string value returns a loaded value
   * (Male/Female).
   */
  @Test
  public void testMask() {
    GenderMaskingProviderConfig maskingConfiguration = new GenderMaskingProviderConfig();
    MaskingProvider maskingProvider =
        new GenderMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String originalValue = "male";
    int randomizationOK = 0;

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(originalValue);
      assertTrue(maskedValue, "Male".equals(maskedValue) || "Female".equals(maskedValue));
      if (!maskedValue.equalsIgnoreCase(originalValue)) {
        randomizationOK++;
      }
    }
    assertTrue(randomizationOK > 0);

    originalValue = "NotAGenderXXXX";
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(originalValue);
      assertTrue(maskedValue, "Male".equals(maskedValue) || "Female".equals(maskedValue));
    }
  }
}
