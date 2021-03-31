/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.CountryIdentifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.shared.pojo.config.masking.CountryMaskingProviderConfig;
import com.ibm.whc.deid.util.CountryManager;
import com.ibm.whc.deid.util.CountryNameSpecification;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class CountryMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {
  private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;
  /*
   * Tests all three of the CountryMaskingProvider options (country.mask.closest,
   * country.mask.closestK, and country.mask.pseudorandom). When the country.mask.closest flag is
   * true, it uses the country.mask.closestK default value (10). It uses the Localization resource
   * to identify the list of related countries to the original value for verification.
   */

  @Test
  public void testRandomCountryGenerator() throws Exception {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    MaskingProvider countryMaskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String originalCountry = "United Kingdom";

    int randomizationOK = 0;

    for (int i = 0; i < 1000; i++) {
      String randomCountry = countryMaskingProvider.mask(originalCountry);
      if (!randomCountry.toUpperCase().equals(originalCountry.toUpperCase())) {
        randomizationOK++;
      }
    }

    assertTrue(randomizationOK > 0);
  }

  @Test
  public void testPseudorandom() throws Exception {

    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    maskingConfiguration.setMaskPseudorandom(true);

    MaskingProvider maskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String originalCity = "Italy";
    String maskedCity = maskingProvider.mask(originalCity);

    String firstMask = maskedCity;

    for (int i = 0; i < 100; i++) {
      maskedCity = maskingProvider.mask(originalCity);
      // System.out.println(maskedCity);
      assertEquals(firstMask, maskedCity);
    }
  }



  @Test
  public void testEmptyValue() {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    MaskingProvider countryMaskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String originalCountry = "";
    String randomCountry = countryMaskingProvider.mask(originalCountry);

    assertEquals(null, randomCountry);
  }

  @Test
  public void testPreservesFormat() {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    MaskingProvider countryMaskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);
    CountryManager countryManager = new CountryManager(null, localizationProperty);

    String originalCountry = "GB";
    String randomCountry = countryMaskingProvider.mask(originalCountry);
    // assertFalse(randomCountry.equals(originalCountry));
    assertTrue(countryManager.isValidCountry(randomCountry, CountryNameSpecification.ISO2));

    originalCountry = "ITA";
    randomCountry = countryMaskingProvider.mask(originalCountry);
    // assertFalse(randomCountry.equals(originalCountry));
    assertTrue(countryManager.isValidCountry(randomCountry, CountryNameSpecification.ISO3));

    originalCountry = "ITALY";
    randomCountry = countryMaskingProvider.mask(originalCountry);
    // assertFalse(randomCountry.equals(originalCountry));
    assertTrue(countryManager.isValidCountry(randomCountry, CountryNameSpecification.NAME));
  }

  @Test
  public void testClosestCountry() throws Exception {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    maskingConfiguration.setMaskClosest(true);

    MaskingProvider countryMaskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    int randomizationOK = 0;

    for (int i = 0; i < 100; i++) {
      String originalCountry = "GB";
      String randomCountry = countryMaskingProvider.mask(originalCountry);
      if (!randomCountry.equals(originalCountry)) {
        randomizationOK++;
      }
    }

    assertTrue(randomizationOK > 0);
  }

  @Test
  public void testMaskNullCountryInputReturnNull() throws Exception {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    MaskingProvider maskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String invalidCountry = null;
    String maskedCountry = maskingProvider.mask(invalidCountry);

    assertEquals(null, maskedCountry);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCountryInputValidHandlingReturnNull() throws Exception {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String invalidCountry = "Invalid Country";
    String maskedCountry = maskingProvider.mask(invalidCountry);

    assertEquals(null, maskedCountry);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCountryInputValidHandlingReturnRandom() throws Exception {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);
    Identifier identifier = new CountryIdentifier(tenantId, localizationProperty);

    String invalidCountry = "Invalid Country";
    String maskedCountry = maskingProvider.mask(invalidCountry);

    assertFalse(maskedCountry.equals(invalidCountry));
    assertTrue(identifier.isOfThisType(maskedCountry));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCountryInputValidHandlingReturnDefaultCustomValue() throws Exception {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String invalidCountry = "Invalid Country";
    String maskedCountry = maskingProvider.mask(invalidCountry);

    assertEquals("OTHER", maskedCountry);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCountryInputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(3);
    maskingConfiguration.setUnspecifiedValueReturnMessage("Test Country");
    MaskingProvider maskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String invalidCountry = "Invalid Country";
    String maskedCountry = maskingProvider.mask(invalidCountry);

    assertEquals("Test Country", maskedCountry);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCountryInputInvalidHandlingReturnNull() throws Exception {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String invalidCountry = "Invalid Country";
    String maskedCountry = maskingProvider.mask(invalidCountry);

    assertEquals(null, maskedCountry);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  @Ignore
  public void testPerformance() {
    int N = 1000000;
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    CountryMaskingProviderConfig closestConfiguration = new CountryMaskingProviderConfig();
    closestConfiguration.setMaskClosest(true);

    CountryMaskingProviderConfig[] configurations =
        new CountryMaskingProviderConfig[] {maskingConfiguration, closestConfiguration};

    String[] originalValues = new String[] {"GB"};

    for (CountryMaskingProviderConfig config : configurations) {
      CountryMaskingProvider maskingProvider =
          new CountryMaskingProvider(config, tenantId, localizationProperty);

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
