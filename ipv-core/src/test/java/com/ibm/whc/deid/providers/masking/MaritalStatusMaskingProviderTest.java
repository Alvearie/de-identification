/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertTrue;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.MaritalStatusIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.MaritalStatusMaskingProviderConfig;

public class MaritalStatusMaskingProviderTest implements MaskingProviderTest {

  @Test
  public void testMask() {
    MaritalStatusMaskingProviderConfig maskingConfiguration =
        new MaritalStatusMaskingProviderConfig();
    MaritalStatusMaskingProvider maskingProvider =
        new MaritalStatusMaskingProvider(maskingConfiguration, tenantId, localizationProperty);
    MaritalStatusIdentifier identifier =
        new MaritalStatusIdentifier(tenantId, localizationProperty);

    String originalStatus = "XXX";
    for (int i = 0; i < 20; i++) {
      String maskedStatus = maskingProvider.mask(originalStatus);
      assertTrue(identifier.isOfThisType(maskedStatus));
    }
  }

  @Test
  @Ignore
  public void testPerformance() {
    int N = 1000000;
    MaritalStatusMaskingProviderConfig defaultConfiguration =
        new MaritalStatusMaskingProviderConfig();

    MaritalStatusMaskingProviderConfig[] configurations =
        new MaritalStatusMaskingProviderConfig[] {defaultConfiguration};

    String[] originalValues = new String[] {"Married"};

    for (MaritalStatusMaskingProviderConfig maskingConfiguration : configurations) {
      MaritalStatusMaskingProvider maskingProvider =
          new MaritalStatusMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

      for (String originalValue : originalValues) {
        long startMillis = System.currentTimeMillis();

        for (int i = 0; i < N; i++) {
          maskingProvider.mask(originalValue);
        }

        long diff = System.currentTimeMillis() - startMillis;
        System.out.println(String.format("%s: %s: %d operations took %d milliseconds (%f per op)",
            originalValue, N, diff, (double) diff / N));
        // Assert test always should finish in less than 10 seconds
        assertTrue(diff < 10000);
      }
    }
  }
}
