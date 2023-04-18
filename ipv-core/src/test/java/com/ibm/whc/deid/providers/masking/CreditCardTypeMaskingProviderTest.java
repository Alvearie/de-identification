/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.CreditCardTypeIdentifier;

public class CreditCardTypeMaskingProviderTest implements MaskingProviderTest {
  
  @Test
  public void testMask() {
    CreditCardTypeMaskingProvider maskingProvider =
        new CreditCardTypeMaskingProvider(tenantId, localizationProperty);
    CreditCardTypeIdentifier identifier =
        new CreditCardTypeIdentifier(tenantId, localizationProperty);

    String originalValue = "visa";
    int nonMatches = 0;
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(originalValue);
      assertTrue(identifier.isOfThisType(maskedValue));
      if (!maskedValue.equalsIgnoreCase(originalValue)) {
        nonMatches++;
      }
    }
    assertTrue(nonMatches > 0);

    originalValue = "not-recognized";
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(originalValue);
      assertTrue(identifier.isOfThisType(maskedValue));
    }
  }
}
