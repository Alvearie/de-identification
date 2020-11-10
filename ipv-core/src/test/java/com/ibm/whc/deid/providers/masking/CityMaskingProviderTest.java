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
import java.io.InputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.CityIdentifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.CityMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.util.Readers;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;

public class CityMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  protected static BasicMaskingProviderFactory maskingProviderFactory;

  @BeforeClass
  public static void setup() {
    maskingProviderFactory = new BasicMaskingProviderFactory();
  }

  @AfterClass
  public static void cleanup() {
    maskingProviderFactory.invalidateCache(tenantId);
  }

  /*
   * Tests all three of the CityMaskingProvider options (city.mask.closest, city.mask.closestK, and
   * city.mask.pseudorandom). When the city.mask.closest flag is true, it uses the
   * city.mask.closestK default value (10). In addition to locally defined list of related cities,
   * It also uses the Localization resources to identify the list of related cities to the original
   * value for verification.
   */

  @Test
  public void testMask() {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    CityMaskingProvider maskingProvider = (CityMaskingProvider) maskingProviderFactory
        .getProviderFromType(MaskingProviderType.CITY, null, maskingConfiguration, tenantId);

    // different values
    String originalCity = "Dublin";
    String maskedCity = maskingProvider.mask(originalCity);
    assertFalse(originalCity.equalsIgnoreCase(maskedCity));
  }

  @Test
  public void testPseudorandom() throws Exception {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    maskingConfiguration.setMaskPseudorandom(true);

    CityMaskingProvider maskingProvider = (CityMaskingProvider) maskingProviderFactory
        .getProviderFromType(MaskingProviderType.CITY, null, maskingConfiguration, tenantId);

    String originalCity = "Dublin";
    String maskedCity = maskingProvider.mask(originalCity);

    String firstMask = maskedCity;

    for (int i = 0; i < 100; i++) {
      maskedCity = maskingProvider.mask(originalCity);
      assertEquals(firstMask, maskedCity);
    }
  }

  @Test
  public void testPseudorandomLocalization() throws Exception {

    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    maskingConfiguration.setMaskPseudorandom(true);

    CityMaskingProvider maskingProvider = (CityMaskingProvider) maskingProviderFactory
        .getProviderFromType(MaskingProviderType.CITY, null, maskingConfiguration, tenantId);

    Collection<ResourceEntry> entryCollection = LocalizationManager.getInstance()
        .getResources(Resource.CITY, Collections.singletonList("gr"));
    Set<String> greekValues = new HashSet<>();

    for (ResourceEntry entry : entryCollection) {
      InputStream inputStream = entry.createStream();
      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String name = line.get(0);
          greekValues.add(name.toUpperCase());
        }
        inputStream.close();
      }
    }

    String originalCity = "Πάτρα";
    String maskedCity = maskingProvider.mask(originalCity);

    String firstMask = maskedCity;

    for (int i = 0; i < 100; i++) {
      maskedCity = maskingProvider.mask(originalCity);
      assertEquals(firstMask, maskedCity);
      assertTrue(greekValues.contains(maskedCity.toUpperCase()));
    }
  }

  @Test
  public void testLocalization() throws Exception {

    // this test assumes that GR is loaded by default

    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    CityMaskingProvider maskingProvider = (CityMaskingProvider) maskingProviderFactory
        .getProviderFromType(MaskingProviderType.CITY, null, maskingConfiguration, tenantId);
    String greekOriginalValue = "Πάτρα";

    Collection<ResourceEntry> entryCollection = LocalizationManager.getInstance()
        .getResources(Resource.CITY, Collections.singletonList("gr"));
    Set<String> greekValues = new HashSet<>();

    for (ResourceEntry entry : entryCollection) {
      InputStream inputStream = entry.createStream();
      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String name = line.get(0);
          greekValues.add(name.toUpperCase());
        }
        inputStream.close();
      }
    }

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(greekOriginalValue);
      assertTrue(greekValues.contains(maskedValue.toUpperCase()));
    }
  }

  @Ignore
  @Test
  public void testLocalizationClosest() throws Exception {
    // this test assumes that GR is loaded by default
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    maskingConfiguration.setMaskClosest(true);

    CityMaskingProvider maskingProvider = (CityMaskingProvider) maskingProviderFactory
        .getProviderFromType(MaskingProviderType.CITY, null, maskingConfiguration, tenantId);
    String greekOriginalValue = "Πάτρα";

    Collection<ResourceEntry> entryCollection = LocalizationManager.getInstance()
        .getResources(Resource.CITY, Collections.singletonList("gr"));
    Set<String> greekValues = new HashSet<>();

    for (ResourceEntry entry : entryCollection) {
      InputStream inputStream = entry.createStream();
      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String name = line.get(0);
          greekValues.add(name.toUpperCase());
        }
        inputStream.close();
      }
    }

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(greekOriginalValue);
      System.out.println(maskedValue);
      assertTrue(greekValues.contains(maskedValue.toUpperCase()));
    }
  }

  @Test
  public void testMaskNullCityInputReturnNull() throws Exception {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    CityMaskingProvider maskingProvider = (CityMaskingProvider) maskingProviderFactory
        .getProviderFromType(MaskingProviderType.CITY, null, maskingConfiguration, tenantId);

    String invalidCity = null;
    String maskedCity = maskingProvider.mask(invalidCity);

    assertEquals(null, maskedCity);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCityInputValidHandlingReturnNull() throws Exception {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    CityMaskingProvider maskingProvider = (CityMaskingProvider) maskingProviderFactory
        .getProviderFromType(MaskingProviderType.CITY, null, maskingConfiguration, tenantId);
    String invalidCity = "Invalid City";
    String maskedCity = maskingProvider.mask(invalidCity);

    assertEquals(null, maskedCity);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCityInputValidHandlingReturnRandom() throws Exception {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(2);
    CityMaskingProvider maskingProvider = (CityMaskingProvider) maskingProviderFactory
        .getProviderFromType(MaskingProviderType.CITY, null, maskingConfiguration, tenantId);
    Identifier identifier = new CityIdentifier();

    String invalidCity = "Invalid City";
    String maskedCity = maskingProvider.mask(invalidCity);

    assertFalse(maskedCity.equals(invalidCity));
    assertTrue(identifier.isOfThisType(maskedCity));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCityInputValidHandlingReturnDefaultCustomValue() throws Exception {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(3);
    CityMaskingProvider maskingProvider = (CityMaskingProvider) maskingProviderFactory
        .getProviderFromType(MaskingProviderType.CITY, null, maskingConfiguration, tenantId);
    String invalidCity = "Invalid City";
    String maskedCity = maskingProvider.mask(invalidCity);

    assertEquals("OTHER", maskedCity);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCityInputValidHandlingReturnNonDefaultCustomValue() throws Exception {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(3);
    maskingConfiguration.setUnspecifiedValueReturnMessage("Test City");
    CityMaskingProvider maskingProvider = (CityMaskingProvider) maskingProviderFactory
        .getProviderFromType(MaskingProviderType.CITY, null, maskingConfiguration, tenantId);

    String invalidCity = "Invalid City";
    String maskedCity = maskingProvider.mask(invalidCity);

    assertEquals("Test City", maskedCity);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCityInputInvalidHandlingReturnNull() throws Exception {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(4);
    CityMaskingProvider maskingProvider = (CityMaskingProvider) maskingProviderFactory
        .getProviderFromType(MaskingProviderType.CITY, null, maskingConfiguration, tenantId);
    String invalidCity = "Invalid City";
    String maskedCity = maskingProvider.mask(invalidCity);

    assertEquals(null, maskedCity);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  @Ignore
  public void testPerformance() {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    CityMaskingProviderConfig closestConfiguration = new CityMaskingProviderConfig();
    closestConfiguration.setMaskClosest(true);

    CityMaskingProviderConfig[] configurations = {maskingConfiguration, closestConfiguration};

    String originalCity = "Dublin";

    for (CityMaskingProviderConfig config : configurations) {
      CityMaskingProvider maskingProvider = new CityMaskingProvider(config, tenantId);

      int N = 1000000;

      long startMillis = System.currentTimeMillis();

      for (int i = 0; i < N; i++) {
        maskingProvider.mask(originalCity);
      }

      long diff = System.currentTimeMillis() - startMillis;
      System.out.println(String.format(" %d operations took %d milliseconds (%f per op)", N, diff,
          (double) diff / N));
      // Assert test always should finish in less than 10 seconds
      assertTrue(diff < 10000);
    }
  }

  @Test
  public void testMaskClosest() {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    maskingConfiguration.setMaskClosest(true);

    CityMaskingProvider maskingProvider = (CityMaskingProvider) maskingProviderFactory
        .getProviderFromType(MaskingProviderType.CITY, null, maskingConfiguration, tenantId);

    String originalCity = "Dublin";
    String[] neighbors = {"Lucan", "Tallaght", "Blanchardstown", "Wolverhampton", "Stoke-on-Trent",
        "Dún Laoghaire", "Manchester", "Swords", "Donaghmede", "Warrington", "Preston", "St Helens",
        "Swansea", "Liverpool", "Blackpool", "Cork", "Limerick", "Belfast", "Preston", "Dublin",
        "Glasgow", "Cardiff", "Plymouth"};

    List<String> neighborsList = Arrays.asList(neighbors);

    for (int i = 0; i < 1000; i++) {
      String maskedCity = maskingProvider.mask(originalCity);
      assertTrue(neighborsList.contains(maskedCity));
    }
  }

}
