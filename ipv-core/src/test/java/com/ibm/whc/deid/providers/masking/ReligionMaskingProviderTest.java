/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Ignore;
import org.junit.Test;

import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.ReligionIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.ReligionMaskingProviderConfig;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class ReligionMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {
  // @formatter:off
  /*
   * Religion masking provider has no options. It tests for random masking of
   * religion value. It also tests for localization masking of religion value.
   */

	private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

  @Test
  public void testMask() throws Exception {
    ReligionMaskingProviderConfig configuration = new ReligionMaskingProviderConfig();
		MaskingProvider maskingProvider = new ReligionMaskingProvider(configuration, tenantId, localizationProperty);
    ReligionIdentifier identifier = new ReligionIdentifier();

    String originalReligion = "Buddhist";

    int randomizationOK = 0;

    for (int i = 0; i < 10; i++) {
      String maskedReligion = maskingProvider.mask(originalReligion);
      assertTrue(identifier.isOfThisType(maskedReligion));
      if (!maskedReligion.equals(originalReligion)) {
        randomizationOK++;
      }
    }
    assertTrue(randomizationOK > 0);
  }


  @Test
  public void testMaskNullReligionInputReturnNull() throws Exception {
    ReligionMaskingProviderConfig configuration = new ReligionMaskingProviderConfig();
		MaskingProvider maskingProvider = new ReligionMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidReligion = null;
    String maskedReligion = maskingProvider.mask(invalidReligion);

    assertEquals(null, maskedReligion);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidReligionInputValidHandlingReturnNull() throws Exception {
    ReligionMaskingProviderConfig configuration = new ReligionMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
		MaskingProvider maskingProvider = new ReligionMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidReligion = "Invalid Religion";
    String maskedReligion = maskingProvider.mask(invalidReligion);

    assertEquals(null, maskedReligion);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidReligionInputValidHandlingReturnRandom() throws Exception {
    ReligionMaskingProviderConfig configuration = new ReligionMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
		MaskingProvider maskingProvider = new ReligionMaskingProvider(configuration, tenantId, localizationProperty);
    Identifier identifier = new ReligionIdentifier();

    String invalidReligion = "Invalid Religion";
    String maskedReligion = maskingProvider.mask(invalidReligion);

    assertFalse(maskedReligion.equals(invalidReligion));
    assertTrue(identifier.isOfThisType(maskedReligion));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidReligionInputValidHandlingReturnDefaultCustomValue() throws Exception {
    ReligionMaskingProviderConfig configuration = new ReligionMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
		MaskingProvider maskingProvider = new ReligionMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidReligion = "Invalid Religion";
    String maskedReligion = maskingProvider.mask(invalidReligion);

    assertEquals("OTHER", maskedReligion);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidReligionInputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    ReligionMaskingProviderConfig configuration = new ReligionMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("Test Religion");
		MaskingProvider maskingProvider = new ReligionMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidReligion = "Invalid Religion";
    String maskedReligion = maskingProvider.mask(invalidReligion);

    assertEquals("Test Religion", maskedReligion);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidReligionInputInvalidHandlingReturnNull() throws Exception {
    ReligionMaskingProviderConfig configuration = new ReligionMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
		MaskingProvider maskingProvider = new ReligionMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidReligion = "Invalid Religion";
    String maskedReligion = maskingProvider.mask(invalidReligion);

    assertEquals(null, maskedReligion);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
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
