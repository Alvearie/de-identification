/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.CreditCardTypeIdentifier;

public class CreditCardTypeMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {
  
  @Test
  public void testMask() {
    CreditCardTypeMaskingProvider maskingProvider =
        new CreditCardTypeMaskingProvider(tenantId, localizationProperty);
    CreditCardTypeIdentifier identifier =
        new CreditCardTypeIdentifier(tenantId, localizationProperty);

    String originalValue = "VISA";

    int nonMatches = 0;
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(originalValue);
      assertTrue(identifier.isOfThisType(maskedValue));
      if (!maskedValue.equals(originalValue)) {
        nonMatches++;
      }
    }

    assertTrue(nonMatches > 0);
  }
}
