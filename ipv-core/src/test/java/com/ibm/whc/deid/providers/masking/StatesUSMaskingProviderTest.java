/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.StatesUSIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.StatesUSMaskingProviderConfig;

public class StatesUSMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  @Test
  public void testMask() {
    Identifier statesUSIdentifier = new StatesUSIdentifier(tenantId, localizationProperty);
    StatesUSMaskingProviderConfig maskingConfiguration = new StatesUSMaskingProviderConfig();
    MaskingProvider maskingProvider =
        new StatesUSMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String value = "Alabama";
    int randomizationOK = 0;

    for (int i = 0; i < 20; i++) {
      String maskedValue = maskingProvider.mask(value);
      assertTrue(statesUSIdentifier.isOfThisType(maskedValue));
      if (!maskedValue.equals(value)) {
        randomizationOK++;
      }
      // should return full name since input was a recognized full name
      assertNotEquals(2, maskedValue.length());
    }
    assertTrue(randomizationOK > 0);

    value = "AL";
    randomizationOK = 0;

    for (int i = 0; i < 20; i++) {
      String maskedValue = maskingProvider.mask(value);
      assertTrue(statesUSIdentifier.isOfThisType(maskedValue));
      if (!maskedValue.equals(value)) {
        randomizationOK++;
      }
      // should return abbreviation since input was a recognized abbreviation
      assertEquals(2, maskedValue.length());
    }
    assertTrue(randomizationOK > 0);

    value = "unknown";
    String maskedValue = maskingProvider.mask(value);
    assertNotNull(maskedValue);
    assertTrue(statesUSIdentifier.isOfThisType(maskedValue));
  }

  @Test
  public void testMaskEmptyValue() {
    Identifier statesUSIdentifier = new StatesUSIdentifier(tenantId, localizationProperty);
    StatesUSMaskingProviderConfig maskingConfiguration = new StatesUSMaskingProviderConfig();
    MaskingProvider maskingProvider =
        new StatesUSMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String value = "";

    String maskedValue = maskingProvider.mask(value);
    assertTrue(statesUSIdentifier.isOfThisType(maskedValue));
  }
}
