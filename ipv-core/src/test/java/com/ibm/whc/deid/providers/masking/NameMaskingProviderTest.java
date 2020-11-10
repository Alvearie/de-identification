/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.io.InputStream;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.NameIdentifier;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.NameMaskingProviderConfig;
import com.ibm.whc.deid.util.NamesManager;
import com.ibm.whc.deid.util.Readers;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;

public class NameMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {
  private final NamesManager.NameManager names = new NamesManager.NameManager(null);

  @Test
  public void testMask() throws Exception {
    NameMaskingProviderConfig defaultConfiguration = new NameMaskingProviderConfig();
    NameMaskingProvider nameMaskingProvider =
        new NameMaskingProvider(defaultConfiguration, tenantId);

    String name = "John";
    String res = nameMaskingProvider.mask(name);
    assertFalse(name.equals(res));
    // After enabling the "names.masking.genderPreserve", the Gender can
    // change so commenting out some Asserts
    // assertTrue(names.getGender(name) == names.getGender(res));

    // capitalization shouldn't matter
    name = "JoHn";
    res = nameMaskingProvider.mask(name);
    assertFalse(name.equals(res));
    // assertTrue(names.getGender(name) == names.getGender(res));

    // female detection
    name = "Mary";
    res = nameMaskingProvider.mask(name);
    assertFalse(name.equals(res));
    // assertTrue(names.getGender(name) == names.getGender(res));

    // surname detection
    name = "Smith";
    res = nameMaskingProvider.mask(name);
    assertFalse(name.equals(res));
    assertTrue(names.isLastName(res));
  }

  @Test
  public void testLocalizationFirstName() throws Exception {
    // this test assumes that GR is loaded by default

    NameMaskingProviderConfig defaultConfiguration = new NameMaskingProviderConfig();
    NameMaskingProvider maskingProvider = new NameMaskingProvider(defaultConfiguration, tenantId);

    String greekOriginalValue = "Γιώργος";

    Collection<ResourceEntry> entryCollection = LocalizationManager.getInstance()
        .getResources(Resource.FIRST_NAME_MALE, Collections.singletonList("gr"));
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

    entryCollection = LocalizationManager.getInstance().getResources(Resource.FIRST_NAME_FEMALE,
        Collections.singletonList("gr"));

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
  public void testLocalizationLastName() throws Exception {
    // this test assumes that GR is loaded by default
    NameMaskingProviderConfig defaultConfiguration = new NameMaskingProviderConfig();
    NameMaskingProvider maskingProvider = new NameMaskingProvider(defaultConfiguration, tenantId);
    String greekOriginalValue = "Παπαδόπουλος";

    Collection<ResourceEntry> entryCollection = LocalizationManager.getInstance()
        .getResources(Resource.LAST_NAME, Collections.singletonList("gr"));
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
  public void testMaskNameAndSurname() throws Exception {
    NameMaskingProviderConfig defaultConfiguration = new NameMaskingProviderConfig();
    NameMaskingProvider nameMaskingProvider =
        new NameMaskingProvider(defaultConfiguration, tenantId);

    String name = "John Smith";
    String res = nameMaskingProvider.mask(name);
    assertFalse(name.equals(res));
    assertEquals(2, res.split(" ").length);
  }

  @Test
  @Ignore
  public void testPerformance() {
    int N = 1000000;
    NameMaskingProviderConfig defaultConfiguration = new NameMaskingProviderConfig();
    NameMaskingProviderConfig unisexConfiguration = new NameMaskingProviderConfig();
    unisexConfiguration.setMaskingAllowUnisex(true);

    NameMaskingProviderConfig[] configurations =
        new NameMaskingProviderConfig[] {defaultConfiguration, unisexConfiguration};

    String[] originalValues = new String[] {"John"};

    for (NameMaskingProviderConfig maskingConfiguration : configurations) {
      NameMaskingProvider maskingProvider = new NameMaskingProvider(maskingConfiguration, tenantId);

      for (String originalValue : originalValues) {
        long startMillis = System.currentTimeMillis();

        for (int i = 0; i < N; i++) {
          maskingProvider.mask(originalValue);
        }

        long diff = System.currentTimeMillis() - startMillis;
        System.out.println(String.format(" %s: %d operations took %d milliseconds (%f per op)",
            originalValue, N, diff, (double) diff / N));
        // Assert test always should finish in less than 10 seconds
        assertTrue(diff < 10000);
      }
    }
  }

  @Test
  public void testMaskUnisexAllowed() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setMaskingAllowUnisex(true);

    MaskingProvider nameMaskingProvider = new NameMaskingProvider(configuration, tenantId);

    String name = "Mary";

    for (int i = 0; i < 100; i++) {
      String res = nameMaskingProvider.mask(name);
      assertFalse(name.equals(res));
      // assertTrue(names.getGender(name) == names.getGender(res) ||
      // (names.getGender(res) == Gender.both));
    }
  }

  @Test
  public void testMaskNullNameInputReturnNull() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setMaskingAllowUnisex(true);
    MaskingProvider maskingProvider = new NameMaskingProvider(configuration, tenantId);

    String invalidName = null;
    String maskedName = maskingProvider.mask(invalidName);

    assertEquals(null, maskedName);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNameInputValidHandlingReturnNull() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);

    MaskingProvider maskingProvider = new NameMaskingProvider(configuration, tenantId);

    String invalidName = "Invalid Name";
    String maskedName = maskingProvider.mask(invalidName);

    assertEquals(null, maskedName);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNameInputValidHandlingReturnRandom() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider = new NameMaskingProvider(configuration, tenantId);
    Identifier identifier = new NameIdentifier();

    String invalidName = "Invalid Name";
    String maskedName = maskingProvider.mask(invalidName);

    assertFalse(maskedName.equals(invalidName));
    assertTrue(identifier.isOfThisType(maskedName));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNameInputValidHandlingReturnDefaultCustomValue() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new NameMaskingProvider(configuration, tenantId);

    String invalidName = "Invalid Name";
    String maskedName = maskingProvider.mask(invalidName);

    assertEquals("OTHER", maskedName);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNameInputValidHandlingReturnNonDefaultCustomValue() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("Test Name");
    MaskingProvider maskingProvider = new NameMaskingProvider(configuration, tenantId);

    String invalidName = "Invalid Name";
    String maskedName = maskingProvider.mask(invalidName);

    assertEquals("Test Name", maskedName);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNameInputInvalidHandlingReturnNull() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new NameMaskingProvider(configuration, tenantId);

    String invalidName = "Invalid Name";
    String maskedName = maskingProvider.mask(invalidName);

    assertEquals(null, maskedName);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testTokenConsistence() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setTokenConsistence(true);
    MaskingProvider mp = new NameMaskingProvider(configuration, tenantId);

    String name1 = "Mary Jane Smith";
    String name2 = "Mary Smith";
    String name3 = "Mary J. Smith";

    String masked1 = mp.mask(name1);
    String masked2 = mp.mask(name2);
    String masked3 = mp.mask(name3);

    assertNotEquals(masked1, name1);
    assertNotEquals(masked2, name2);
    assertNotEquals(masked3, name3);

    assertNotEquals(masked1, masked2);
    assertNotEquals(masked1, masked3);
    assertNotEquals(masked2, masked3);

    String[] m1 = masked1.split(" "), m2 = masked2.split(" "), m3 = masked3.split(" ");

    // name
    assertThat(m1[0], is(m2[0]));
    assertThat(m1[0], is(m3[0]));
    assertThat(m2[0], is(m3[0]));

    // surname
    assertThat(m1[2], is(m2[1]));
    assertThat(m1[2], is(m3[m3.length - 1]));
    assertThat(m2[1], is(m3[m3.length - 1]));
  }

  @Test
  public void pseudoRandom() throws Exception {
    String original1 = "John Doe";
    String original2 = "John James Smith";

    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setTokenConsistence(false);
    configuration.setMaskPseudorandom(true);

    MaskingProvider mp = new NameMaskingProvider(configuration, tenantId);

    String maskedName1 = mp.mask(original1);
    String maskedName2 = mp.mask(original2);

    assertEquals(maskedName1.split(" ")[0], maskedName2.split(" ")[0]);
  }

}
