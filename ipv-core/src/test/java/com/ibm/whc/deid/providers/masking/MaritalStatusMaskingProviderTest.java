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
import java.util.HashSet;
import java.util.Set;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.MaritalStatusIdentifier;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.MaritalStatusMaskingProviderConfig;
import com.ibm.whc.deid.util.Readers;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;

public class MaritalStatusMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  @Test
  public void testMask() {
    MaritalStatusMaskingProviderConfig maskingConfiguration =
        new MaritalStatusMaskingProviderConfig();
    MaritalStatusMaskingProvider maskingProvider =
        new MaritalStatusMaskingProvider(maskingConfiguration, tenantId);
    MaritalStatusIdentifier identifier = new MaritalStatusIdentifier();

    String originalStatus = "Married";

    int randomizationOK = 0;

    for (int i = 0; i < 10; i++) {
      String maskedStatus = maskingProvider.mask(originalStatus);
      assertTrue(identifier.isOfThisType(maskedStatus));
      if (!maskedStatus.equals(originalStatus)) {
        randomizationOK++;
      }
    }
    assertTrue(randomizationOK > 0);
  }

  @Test
  public void testLocalization() throws Exception {
    // this test assumes that GR is loaded by default

    MaritalStatusMaskingProviderConfig maskingConfiguration =
        new MaritalStatusMaskingProviderConfig();
    MaskingProvider maskingProvider =
        new MaritalStatusMaskingProvider(maskingConfiguration, tenantId);

    String greekOriginalValue = "Χήρα";

    Collection<ResourceEntry> entryCollection = LocalizationManager.getInstance()
        .getResources(Resource.MARITAL_STATUS, Arrays.asList(new String[] {"gr"}));
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

  @Test
  public void testMaskNullMaritalStatusInputReturnNull() throws Exception {
    MaritalStatusMaskingProviderConfig maskingConfiguration =
        new MaritalStatusMaskingProviderConfig();
    MaskingProvider maskingProvider =
        new MaritalStatusMaskingProvider(maskingConfiguration, tenantId);

    String invalidMaritalStatus = null;
    String maskedMaritalStatus = maskingProvider.mask(invalidMaritalStatus);

    assertEquals(null, maskedMaritalStatus);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidMaritalStatusInputValidHandlingReturnNull() throws Exception {
    MaritalStatusMaskingProviderConfig maskingConfiguration =
        new MaritalStatusMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider =
        new MaritalStatusMaskingProvider(maskingConfiguration, tenantId);

    String invalidMaritalStatus = "Invalid Marital Status";
    String maskedMaritalStatus = maskingProvider.mask(invalidMaritalStatus);

    assertEquals(null, maskedMaritalStatus);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidMaritalStatusInputValidHandlingReturnRandom() throws Exception {
    MaritalStatusMaskingProviderConfig maskingConfiguration =
        new MaritalStatusMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider =
        new MaritalStatusMaskingProvider(maskingConfiguration, tenantId);
    Identifier identifier = new MaritalStatusIdentifier();

    String invalidMaritalStatus = "Invalid Marital Status";
    String maskedMaritalStatus = maskingProvider.mask(invalidMaritalStatus);

    assertFalse(maskedMaritalStatus.equals(invalidMaritalStatus));
    assertTrue(identifier.isOfThisType(maskedMaritalStatus));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidMaritalStatusInputValidHandlingReturnDefaultCustomValue()
      throws Exception {
    MaritalStatusMaskingProviderConfig maskingConfiguration =
        new MaritalStatusMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider =
        new MaritalStatusMaskingProvider(maskingConfiguration, tenantId);

    String invalidMaritalStatus = "Invalid Marital Status";
    String maskedMaritalStatus = maskingProvider.mask(invalidMaritalStatus);

    assertEquals("OTHER", maskedMaritalStatus);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidMaritalStatusInputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    MaritalStatusMaskingProviderConfig maskingConfiguration =
        new MaritalStatusMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(3);
    maskingConfiguration.setUnspecifiedValueReturnMessage("Test Marital Status");
    MaskingProvider maskingProvider =
        new MaritalStatusMaskingProvider(maskingConfiguration, tenantId);

    String invalidMaritalStatus = "Invalid Marital Status";
    String maskedMaritalStatus = maskingProvider.mask(invalidMaritalStatus);

    assertEquals("Test Marital Status", maskedMaritalStatus);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidMaritalStatusInputInvalidHandlingReturnNull() throws Exception {
    MaritalStatusMaskingProviderConfig maskingConfiguration =
        new MaritalStatusMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider =
        new MaritalStatusMaskingProvider(maskingConfiguration, tenantId);

    String invalidMaritalStatus = "Invalid Marital Status";
    String maskedMaritalStatus = maskingProvider.mask(invalidMaritalStatus);

    assertEquals(null, maskedMaritalStatus);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
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
          new MaritalStatusMaskingProvider(maskingConfiguration, tenantId);

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
