/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import java.util.HashSet;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.models.Country;
import com.ibm.whc.deid.providers.identifiers.CountryIdentifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.shared.pojo.config.masking.CountryMaskingProviderConfig;
import com.ibm.whc.deid.util.CountryManager;
import com.ibm.whc.deid.util.CountryNameSpecification;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class CountryMaskingProviderTest implements MaskingProviderTest {
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
    CountryMaskingProvider countryMaskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String originalCountry = "United Kingdom";
    int randomizationOK = 0;
    for (int i = 0; i < 100; i++) {
      String randomCountry = countryMaskingProvider.mask(originalCountry);
      assertNotNull(randomCountry);
      if (!randomCountry.toUpperCase().equals(originalCountry.toUpperCase())) {
        randomizationOK++;
      }
      Country country = countryMaskingProvider.getCountryManager().getValue(randomCountry);
      assertNotNull(country);
      assertEquals(CountryNameSpecification.NAME, country.getCountryNameSpecification());
    }
    assertTrue(randomizationOK > 0);

    originalCountry = "";
    for (int i = 0; i < 20; i++) {
      String randomCountry = countryMaskingProvider.mask(originalCountry);
      assertNotNull(randomCountry);
      Country country = countryMaskingProvider.getCountryManager().getValue(randomCountry);
      assertNotNull(country);
      assertEquals(CountryNameSpecification.NAME, country.getCountryNameSpecification());
    }
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
  public void testPreservesFormat() {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    CountryMaskingProvider countryMaskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String originalCountry = "GB";
    String randomCountry = countryMaskingProvider.mask(originalCountry);
    // assertFalse(randomCountry.equals(originalCountry));
    assertNotNull(randomCountry);
    assertEquals(2, randomCountry.length());
    Country country = countryMaskingProvider.getCountryManager().getValue(randomCountry);
    assertNotNull(country);
    assertEquals(CountryNameSpecification.ISO2, country.getCountryNameSpecification());

    originalCountry = "ITA";
    randomCountry = countryMaskingProvider.mask(originalCountry);
    // assertFalse(randomCountry.equals(originalCountry));
    assertNotNull(randomCountry);
    assertEquals(3, randomCountry.length());
    country = countryMaskingProvider.getCountryManager().getValue(randomCountry);
    assertNotNull(country);
    assertEquals(randomCountry, CountryNameSpecification.ISO3,
        country.getCountryNameSpecification());
  }

  @Test
  public void testClosestCountry() throws Exception {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    maskingConfiguration.setMaskClosest(true);
    MaskingProvider countryMaskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String originalCountry = "Greece";
    HashSet<String> neighbors =
        new HashSet<>(Arrays.asList("CYPRUS", "MALTA", "TURKEY", "SERBIA", "BOSNIA AND HERZEGOVINA",
            "ROMANIA", "MONTENEGRO", "BULGARIA", "ALBANIA", "GREECE", "HUNGARY",
            "MACEDONIA (THE FORMER YUGOSLAV REPUBLIC OF)"));
    for (int i = 0; i < 30; i++) {
      String randomCountry = countryMaskingProvider.mask(originalCountry);
      assertNotNull(randomCountry);
      assertTrue(neighbors.contains(randomCountry.toUpperCase()));
    }

    originalCountry = "Gr";
    neighbors = new HashSet<>(
        Arrays.asList("CY", "MT", "TR", "RS", "BA", "RO", "ME", "BG", "AL", "GR", "HU", "MK"));
    for (int i = 0; i < 30; i++) {
      String randomCountry = countryMaskingProvider.mask(originalCountry);
      assertTrue(neighbors.contains(randomCountry.toUpperCase()));
    }

    originalCountry = "GrC";
    neighbors = new HashSet<>(
        Arrays.asList("CYP", "MLT", "TUR", "SRB", "BIH", "ROU", "MNE", "BGR", "ALB", "GRC", "HUN",
            "MKD"));
    for (int i = 0; i < 30; i++) {
      String randomCountry = countryMaskingProvider.mask(originalCountry);
      assertTrue(neighbors.contains(randomCountry.toUpperCase()));
    }
  }

  @Test
  public void testMaskNullCountryInputReturnNull() throws Exception {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    maskingConfiguration.setMaskClosest(true);
    MaskingProvider maskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    assertNull(maskingProvider.mask("xx"));
  }

  @Test
  public void testMaskInvalidCountryInputValidHandlingReturnNull() throws Exception {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(1);
    maskingConfiguration.setMaskClosest(true);
    MaskingProvider maskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String invalidCountry = "Invalid Country";
    String maskedCountry = maskingProvider.mask(invalidCountry);

    assertEquals(null, maskedCountry);
  }

  @Test
  public void testMaskInvalidCountryInputValidHandlingReturnRandom() throws Exception {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    maskingConfiguration.setMaskClosest(true);
    maskingConfiguration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);
    Identifier identifier = new CountryIdentifier(tenantId, localizationProperty);

    String invalidCountry = "Invalid Country";
    String maskedCountry = maskingProvider.mask(invalidCountry);

    assertFalse(maskedCountry.equals(invalidCountry));
    assertTrue(identifier.isOfThisType(maskedCountry));
  }

  @Test
  public void testMaskInvalidCountryInputValidHandlingReturnDefaultCustomValue() throws Exception {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    maskingConfiguration.setMaskClosest(true);
    maskingConfiguration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String invalidCountry = "Invalid Country";
    String maskedCountry = maskingProvider.mask(invalidCountry);

    assertEquals("OTHER", maskedCountry);
  }

  @Test
  public void testMaskInvalidCountryInputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    maskingConfiguration.setMaskClosest(true);
    maskingConfiguration.setUnspecifiedValueHandling(3);
    maskingConfiguration.setUnspecifiedValueReturnMessage("Test Country");
    MaskingProvider maskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String invalidCountry = "Invalid Country";
    String maskedCountry = maskingProvider.mask(invalidCountry);

    assertEquals("Test Country", maskedCountry);
  }

  @Test
  public void testMaskInvalidCountryInputInvalidHandlingReturnNull() throws Exception {
    CountryMaskingProviderConfig maskingConfiguration = new CountryMaskingProviderConfig();
    maskingConfiguration.setMaskClosest(true);
    maskingConfiguration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider =
        new CountryMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String invalidCountry = "Invalid Country";
    String maskedCountry = maskingProvider.mask(invalidCountry);

    assertEquals(null, maskedCountry);
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
