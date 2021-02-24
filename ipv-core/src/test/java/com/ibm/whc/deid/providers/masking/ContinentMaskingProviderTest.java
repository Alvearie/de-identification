/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

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
import com.ibm.whc.deid.shared.pojo.config.masking.ContinentMaskingProviderConfig;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class ContinentMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

	private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;


  @Test
  public void testMaskClosest() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setMaskClosest(true);
    configuration.setMaskClosestK(4);
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId, localizationProperty);

    String originalContinent = "Europe";
    HashSet<String> validNeighbors =
        new HashSet<>(Arrays.asList("Africa", "Asia", "South America", "North America"));

    for (int i = 0; i < 100; i++) {
      String maskedContinent = maskingProvider.mask(originalContinent);
      assertTrue(validNeighbors.contains(maskedContinent));
    }
  }

  @Test
  public void testCompoundMask() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId, localizationProperty);

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
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId, localizationProperty);

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
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidContinent = null;
    String maskedContinent = maskingProvider.mask(invalidContinent);

    assertEquals(null, maskedContinent);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidContinentInputValidHandlingReturnNull() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidContinent = "Invalid Continent";
    String maskedContinent = maskingProvider.mask(invalidContinent);

    assertEquals(null, maskedContinent);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidContinentInputValidHandlingReturnRandom() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId, localizationProperty);
		Identifier identifier = new ContinentIdentifier(tenantId, localizationProperty);

    String invalidContinent = "Invalid Continent";
    String maskedContinent = maskingProvider.mask(invalidContinent);

    assertNotNull(maskedContinent);
    assertFalse(maskedContinent.equals(invalidContinent));
    assertTrue(identifier.isOfThisType(maskedContinent));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidContinentInputValidHandlingReturnDefaultCustomValue()
      throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId, localizationProperty);

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
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidContinent = "Invalid Continent";
    String maskedContinent = maskingProvider.mask(invalidContinent);

    assertEquals("Test Continent", maskedContinent);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidContinentInputInvalidHandlingReturnNull() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new ContinentMaskingProvider(configuration, tenantId, localizationProperty);

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
          new ContinentMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

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
