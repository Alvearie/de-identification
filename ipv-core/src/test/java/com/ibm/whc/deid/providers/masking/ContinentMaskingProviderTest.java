/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.models.OriginalMaskedValuePair;
import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.providers.identifiers.ContinentIdentifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.schema.FieldRelationship;
import com.ibm.whc.deid.schema.RelationshipOperand;
import com.ibm.whc.deid.schema.RelationshipType;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.ContinentMaskingProviderConfig;
import com.ibm.whc.deid.util.Readers;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;

public class ContinentMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {
  /*
   * Tests for continent mask closest and closestK options and their respective values. It also
   * tests for localization and compound masking value.
   */

  @Test
  public void testMask() throws Exception {
    // By default, the continent mask closest flag is false and closestK is
    // 5.
    ContinentMaskingProviderConfig maskingConfiguration = new ContinentMaskingProviderConfig();
    MaskingProvider maskingProvider = new ContinentMaskingProvider(maskingConfiguration, tenantId);

    // different values
    String originalContinent = "Europe";
    String maskedContinent = maskingProvider.mask(originalContinent);
    assertFalse(originalContinent.equals(maskedContinent));
  }

  @Test
  public void testLocalization() throws Exception {
    // this test assumes that GR is loaded by default
    // By default, the continent mask closest flag is false and closestK is
    // 5.

    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId);

    // different values
    String originalContinent = "Europe";
    String maskedContinent = maskingProvider.mask(originalContinent);
    assertFalse(originalContinent.equals(maskedContinent));

    String greekContinent = "Ευρώπη";

    Collection<ResourceEntry> entryCollection = LocalizationManager.getInstance()
        .getResources(Resource.CONTINENT, Arrays.asList(new String[] {"gr"}));
    Set<String> greekCities = new HashSet<>();

    for (ResourceEntry entry : entryCollection) {
      InputStream inputStream = entry.createStream();
      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String name = line.get(0);
          greekCities.add(name.toUpperCase());
        }
        inputStream.close();
      }
    }

    for (int i = 0; i < 100; i++) {
      maskedContinent = maskingProvider.mask(greekContinent);
      assertTrue(greekCities.contains(maskedContinent.toUpperCase()));
    }
  }

  @Test
  public void testMaskClosest() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setMaskClosest(true);
    configuration.setMaskClosestK(2);
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId);

    String originalContinent = "Europe";

    String[] validNeighbors = {"Europe", "Africa"};

    List<String> neighborsList = Arrays.asList(validNeighbors);

    for (int i = 0; i < 100; i++) {
      String maskedContinent = maskingProvider.mask(originalContinent);
      assertTrue(neighborsList.contains(maskedContinent));
    }
  }

  @Test
  public void testCompoundMask() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId);

    String originalContinent = "Europe";

    Map<String, OriginalMaskedValuePair> maskedValues = new HashMap<>();
    maskedValues.put("country", new OriginalMaskedValuePair("Italy", "Australia"));

    FieldRelationship fieldRelationship =
        new FieldRelationship(ValueClass.LOCATION, RelationshipType.LINKED, "field0",
            new RelationshipOperand[] {new RelationshipOperand("country", ProviderType.COUNTRY)});

    for (int i = 0; i < 100; i++) {
      String maskedContinent =
          maskingProvider.mask(originalContinent, "field0", fieldRelationship, maskedValues);
      assertEquals("Oceania".toUpperCase(), maskedContinent.toUpperCase());
    }
  }

  @Test
  public void testCompoundMaskLinkWithCity() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId);

    String originalContinent = "Europe";

    Map<String, OriginalMaskedValuePair> maskedValues = new HashMap<>();
    maskedValues.put("city", new OriginalMaskedValuePair("Rome", "Sydney"));

    FieldRelationship fieldRelationship =
        new FieldRelationship(ValueClass.LOCATION, RelationshipType.LINKED, "field0",
            new RelationshipOperand[] {new RelationshipOperand("city", ProviderType.CITY)});

    for (int i = 0; i < 100; i++) {
      String maskedContinent =
          maskingProvider.mask(originalContinent, "field0", fieldRelationship, maskedValues);
      assertEquals("Oceania".toUpperCase(), maskedContinent.toUpperCase());
    }
  }

  @Test
  public void testMaskNullContinentInputReturnNull() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId);

    String invalidContinent = null;
    String maskedContinent = maskingProvider.mask(invalidContinent);

    assertEquals(null, maskedContinent);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidContinentInputValidHandlingReturnNull() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId);

    String invalidContinent = "Invalid Continent";
    String maskedContinent = maskingProvider.mask(invalidContinent);

    assertEquals(null, maskedContinent);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidContinentInputValidHandlingReturnRandom() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId);
    Identifier identifier = new ContinentIdentifier();

    String invalidContinent = "Invalid Continent";
    String maskedContinent = maskingProvider.mask(invalidContinent);

    assertFalse(maskedContinent.equals(invalidContinent));
    assertTrue(identifier.isOfThisType(maskedContinent));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidContinentInputValidHandlingReturnDefaultCustomValue()
      throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId);

    String invalidContinent = "Invalid Continent";
    String maskedContinent = maskingProvider.mask(invalidContinent);

    assertEquals("OTHER", maskedContinent);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidContinentInputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("Test Continent");
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId);

    String invalidContinent = "Invalid Continent";
    String maskedContinent = maskingProvider.mask(invalidContinent);

    assertEquals("Test Continent", maskedContinent);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidContinentInputInvalidHandlingReturnNull() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId);

    String invalidContinent = "Invalid Continent";
    String maskedContinent = maskingProvider.mask(invalidContinent);

    assertEquals(null, maskedContinent);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  @Ignore
  public void testPerformance() {
    ContinentMaskingProviderConfig defaultConfiguration = new ContinentMaskingProviderConfig();
    ContinentMaskingProviderConfig closestConfiguration = new ContinentMaskingProviderConfig();
    closestConfiguration.setMaskClosest(true);

    ContinentMaskingProviderConfig[] configurations =
        new ContinentMaskingProviderConfig[] {defaultConfiguration // ,
        // closestConfiguration
        };

    String originalContinent = "Europe";

    for (ContinentMaskingProviderConfig maskingConfiguration : configurations) {
      ContinentMaskingProvider maskingProvider =
          new ContinentMaskingProvider(maskingConfiguration, tenantId);

      int N = 1000000;

      long startMillis = System.currentTimeMillis();

      for (int i = 0; i < N; i++) {
        maskingProvider.mask(originalContinent);
      }

      long diff = System.currentTimeMillis() - startMillis;
      System.out.println(String.format(" %d operations took %d milliseconds (%f per op)", N, diff,
          (double) diff / N));
      // Assert test always should finish in less than 10 seconds
      assertTrue(diff < 10000);
    }
  }

}
