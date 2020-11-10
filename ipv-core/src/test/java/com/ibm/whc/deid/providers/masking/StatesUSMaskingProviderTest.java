/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.StatesUSIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.StatesUSMaskingProviderConfig;

public class StatesUSMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  @Test
  public void testMask() {
    Identifier statesUSIdentifier = new StatesUSIdentifier();
    StatesUSMaskingProviderConfig maskingConfiguration = new StatesUSMaskingProviderConfig();
    MaskingProvider maskingProvider = new StatesUSMaskingProvider(maskingConfiguration, tenantId);

    String value = "Alabama";
    int randomizationOK = 0;

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      assertTrue(statesUSIdentifier.isOfThisType(maskedValue));

      if (!maskedValue.equals(value)) {
        randomizationOK++;
      }
    }

    assertTrue(randomizationOK > 0);
  }

  @Test
  public void testMaskEmptyValue() {
    Identifier statesUSIdentifier = new StatesUSIdentifier();
    StatesUSMaskingProviderConfig maskingConfiguration = new StatesUSMaskingProviderConfig();
    MaskingProvider maskingProvider = new StatesUSMaskingProvider(maskingConfiguration, tenantId);

    String value = "";

    String maskedValue = maskingProvider.mask(value);
    assertTrue(statesUSIdentifier.isOfThisType(maskedValue));
  }

}
