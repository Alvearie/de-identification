/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertTrue;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.RaceEthnicityIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.RaceEthnicityMaskingProviderConfig;

public class RaceEthnicityMaskingProviderTest implements MaskingProviderTest {
  /*
   * Race masking provider has no options. It tests for random masking of race value.
   */

  @Test
  public void testMask() throws Exception {
    RaceEthnicityMaskingProviderConfig configuration = new RaceEthnicityMaskingProviderConfig();
    MaskingProvider maskingProvider =
        new RaceEthnicityMaskingProvider(configuration, tenantId, localizationProperty);
    RaceEthnicityIdentifier identifier =
        new RaceEthnicityIdentifier(tenantId, localizationProperty);

    String originalRace = "native hawaiian";
    int randomizationOK = 0;
    for (int i = 0; i < 20; i++) {
      String maskedRace = maskingProvider.mask(originalRace);
      assertTrue(identifier.isOfThisType(maskedRace));
      if (!maskedRace.equalsIgnoreCase(originalRace)) {
        randomizationOK++;
      }
    }
    assertTrue(randomizationOK > 0);

    // input data need not be recognized as a loaded resource
    originalRace = "xxxx";
    for (int i = 0; i < 20; i++) {
      String maskedRace = maskingProvider.mask(originalRace);
      assertTrue(identifier.isOfThisType(maskedRace));
    }
  }

  @Test
  @Ignore
  public void testPerformance() {
    int N = 1000000;
    RaceEthnicityMaskingProviderConfig defaultConfiguration =
        new RaceEthnicityMaskingProviderConfig();

    RaceEthnicityMaskingProviderConfig[] configurations =
        new RaceEthnicityMaskingProviderConfig[] {defaultConfiguration};

    String[] originalValues = new String[] {"white"};

    for (RaceEthnicityMaskingProviderConfig maskingConfiguration : configurations) {
      RaceEthnicityMaskingProvider maskingProvider =
          new RaceEthnicityMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

      for (String originalValue : originalValues) {
        long startMillis = System.currentTimeMillis();

        for (int i = 0; i < N; i++) {
          maskingProvider.mask(originalValue);
        }

        long diff = System.currentTimeMillis() - startMillis;
        System.out.println(String.format("%s: %d operations took %d milliseconds (%f per op)",
            originalValue, N, diff, (double) diff / N));
        // Assert test always should finish in less than 10 seconds
        assertTrue(diff < 10000);
      }
    }
  }
}
