/*
 * (C) Copyright IBM Corp. 2016,2021
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

import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.NameIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.NameMaskingProviderConfig;
import com.ibm.whc.deid.util.NamesManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class NameMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {
	private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;
  private final NamesManager.NameManager names = new NamesManager.NameManager(null, localizationProperty);

  @Test
  public void testMask() throws Exception {
    NameMaskingProviderConfig defaultConfiguration = new NameMaskingProviderConfig();
    NameMaskingProvider nameMaskingProvider =
        new NameMaskingProvider(defaultConfiguration, tenantId, localizationProperty);

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
  public void testMaskNameAndSurname() throws Exception {
    NameMaskingProviderConfig defaultConfiguration = new NameMaskingProviderConfig();
    NameMaskingProvider nameMaskingProvider =
        new NameMaskingProvider(defaultConfiguration, tenantId, localizationProperty);

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
      NameMaskingProvider maskingProvider = new NameMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

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

    MaskingProvider nameMaskingProvider = new NameMaskingProvider(configuration, tenantId, localizationProperty);

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
    MaskingProvider maskingProvider = new NameMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidName = null;
    String maskedName = maskingProvider.mask(invalidName);

    assertEquals(null, maskedName);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNameInputValidHandlingReturnNull() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);

    MaskingProvider maskingProvider = new NameMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidName = "Invalid Name";
    String maskedName = maskingProvider.mask(invalidName);

    assertEquals(null, maskedName);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNameInputValidHandlingReturnRandom() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider = new NameMaskingProvider(configuration, tenantId, localizationProperty);
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
    MaskingProvider maskingProvider = new NameMaskingProvider(configuration, tenantId, localizationProperty);

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
    MaskingProvider maskingProvider = new NameMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidName = "Invalid Name";
    String maskedName = maskingProvider.mask(invalidName);

    assertEquals("Test Name", maskedName);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNameInputInvalidHandlingReturnNull() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new NameMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidName = "Invalid Name";
    String maskedName = maskingProvider.mask(invalidName);

    assertEquals(null, maskedName);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void pseudoRandom() throws Exception {
    String original1 = "John Doe";
    String original2 = "John James Smith";

    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setMaskPseudorandom(true);

    MaskingProvider mp = new NameMaskingProvider(configuration, tenantId, localizationProperty);

    String maskedName1 = mp.mask(original1);
    String maskedName2 = mp.mask(original2);

    assertEquals(maskedName1.split(" ")[0], maskedName2.split(" ")[0]);
  }

}
