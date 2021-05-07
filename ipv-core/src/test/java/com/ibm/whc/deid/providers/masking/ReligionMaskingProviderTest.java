/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertTrue;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.ReligionIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.ReligionMaskingProviderConfig;

public class ReligionMaskingProviderTest implements MaskingProviderTest {
  /*
   * Religion masking provider has no options. It tests for random masking of religion value.
   */

  @Test
  public void testMask() throws Exception {
    ReligionMaskingProviderConfig configuration = new ReligionMaskingProviderConfig();
		MaskingProvider maskingProvider = new ReligionMaskingProvider(configuration, tenantId, localizationProperty);
		ReligionIdentifier identifier = new ReligionIdentifier(tenantId, localizationProperty);

    String originalReligion = "Buddhist";
    int randomizationOK = 0;
    for (int i = 0; i < 20; i++) {
      String maskedReligion = maskingProvider.mask(originalReligion);
      assertTrue(identifier.isOfThisType(maskedReligion));
      if (!maskedReligion.equalsIgnoreCase(originalReligion)) {
        randomizationOK++;
      }
    }
    assertTrue(randomizationOK > 0);

    // original need not be recognized
    originalReligion = "XXXX";
    for (int i = 0; i < 20; i++) {
      String maskedReligion = maskingProvider.mask(originalReligion);
      assertTrue(identifier.isOfThisType(maskedReligion));
    }
  }

  @Test
  @Ignore
  public void testPerformance() {
    int N = 1000000;
    ReligionMaskingProviderConfig defaultConfiguration = new ReligionMaskingProviderConfig();

    ReligionMaskingProviderConfig[] configurations =
        new ReligionMaskingProviderConfig[] {defaultConfiguration};

    String[] originalValues = new String[] {"Buddhist"};

    for (ReligionMaskingProviderConfig maskingConfiguration : configurations) {
      ReligionMaskingProvider maskingProvider =
					new ReligionMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

      for (String originalValue : originalValues) {
        long startMillis = System.currentTimeMillis();

        for (int i = 0; i < N; i++) {
          maskingProvider.mask(originalValue);
        }

        long diff = System.currentTimeMillis() - startMillis;
        System.out.println(
            String.format(
                " %s: %d operations took %d milliseconds (%f per op)",
                originalValue, N, diff, (double) diff / N));
        // Assert test always should finish in less than 10 seconds
        assertTrue(diff < 10000);
      }
    }
  }
}
