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
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.OccupationIdentifier;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.OccupationMaskingProviderConfig;
import com.ibm.whc.deid.util.Readers;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;

public class OccupationMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {
  /*
   * Tests mask generalize to occupation category and its boolean values (true and false). It also
   * tests for an invalid value and the localization of the occupation.
   */
  @Test
  public void testMaskRandomOccupation() {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    MaskingProvider maskingProvider = new OccupationMaskingProvider(configuration, tenantId);
    Identifier identifier = new OccupationIdentifier();

    String occupation = "actor";
    String maskedValue = maskingProvider.mask(occupation);
    assertTrue(identifier.isOfThisType(maskedValue));
  }

  @Test
  public void testLocalization() throws Exception {
    // this test assumes that GR is loaded by default

    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    MaskingProvider maskingProvider = new OccupationMaskingProvider(configuration, tenantId);

    String greekOriginalValue = "Χτίστης";

    Collection<ResourceEntry> entryCollection = LocalizationManager.getInstance()
        .getResources(Resource.OCCUPATION, Collections.singletonList("gr"));
    Set<String> greekValues = new HashSet<>();
    Set<String> greekCategories = new HashSet<>();

    for (ResourceEntry entry : entryCollection) {
      InputStream inputStream = entry.createStream();
      try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
        for (CSVRecord line : reader) {
          String name = line.get(0);
          greekValues.add(name.toUpperCase());
          greekCategories.add(line.get(1).toUpperCase());
        }
        inputStream.close();
      }
    }

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(greekOriginalValue);
      assertTrue(greekValues.contains(maskedValue.toUpperCase()));
    }

    configuration = new OccupationMaskingProviderConfig();
    configuration.setMaskGeneralize(true);

    maskingProvider = new OccupationMaskingProvider(configuration, tenantId);

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(greekOriginalValue);
      assertTrue(greekCategories.contains(maskedValue.toUpperCase()));
    }
  }

  @Test
  public void testMaskNullOccupationInputReturnNull() throws Exception {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    MaskingProvider maskingProvider = new OccupationMaskingProvider(configuration, tenantId);

    String invalidOccupation = null;
    String maskedOccupation = maskingProvider.mask(invalidOccupation);

    assertEquals(null, maskedOccupation);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidOccupationInputValidHandlingReturnNull() throws Exception {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider = new OccupationMaskingProvider(configuration, tenantId);

    String invalidOccupation = "Invalid Occupation";
    String maskedOccupation = maskingProvider.mask(invalidOccupation);

    assertEquals(null, maskedOccupation);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidOccupationInputValidHandlingReturnRandom() throws Exception {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider = new OccupationMaskingProvider(configuration, tenantId);
    Identifier identifier = new OccupationIdentifier();

    String invalidOccupation = "Invalid Occupation";
    String maskedOccupation = maskingProvider.mask(invalidOccupation);

    assertFalse(maskedOccupation.equals(invalidOccupation));
    assertTrue(identifier.isOfThisType(maskedOccupation));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidOccupationInputValidHandlingReturnDefaultCustomValue()
      throws Exception {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new OccupationMaskingProvider(configuration, tenantId);

    String invalidOccupation = "Invalid Occupation";
    String maskedOccupation = maskingProvider.mask(invalidOccupation);

    assertEquals("OTHER", maskedOccupation);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidOccupationInputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("Test Occupation");
    MaskingProvider maskingProvider = new OccupationMaskingProvider(configuration, tenantId);

    String invalidOccupation = "Invalid Occupation";
    String maskedOccupation = maskingProvider.mask(invalidOccupation);

    assertEquals("Test Occupation", maskedOccupation);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidOccupationInputInvalidHandlingReturnNull() throws Exception {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new OccupationMaskingProvider(configuration, tenantId);

    String invalidOccupation = "Invalid Occupation";
    String maskedOccupation = maskingProvider.mask(invalidOccupation);

    assertEquals(null, maskedOccupation);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskGeneralizeToCategory() {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    configuration.setMaskGeneralize(true);
    MaskingProvider maskingProvider = new OccupationMaskingProvider(configuration, tenantId);

    String occupation = "actor";
    String maskedValue = maskingProvider.mask(occupation);
    assertTrue(maskedValue.equals("Actors, entertainers and presenters"));
  }

  @Test
  public void testMaskGeneralizeToRandomCategoryIfNotFound() {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    configuration.setMaskGeneralize(true);
    MaskingProvider maskingProvider = new OccupationMaskingProvider(configuration, tenantId);

    String occupation = "adadad";
    String maskedValue = maskingProvider.mask(occupation);
    assertEquals(null, maskedValue);
  }

}
