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

import org.junit.Test;

import com.ibm.whc.deid.providers.identifiers.GenderIdentifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.shared.pojo.config.masking.GenderMaskingProviderConfig;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class GenderMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

	private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;
  /*
   * Note: Tries origin value of "male, Female, or Both" 100 times, and gets back a random
   * distribution of masked MALE and FEMALE values. GenderMaskingProvider does not check the origin
   * value for a valid gender value (male, female, F, M, man/men woman/women, both) and for any
   * origin string value returns MALE/FEMALE.
   */
  @Test
  public void testMask() {
    Identifier identifier = new GenderIdentifier();
    GenderMaskingProviderConfig maskingConfiguration = new GenderMaskingProviderConfig();
    MaskingProvider maskingProvider = new GenderMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String originalValue = "male";
    int randomizationOK = 0;

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(originalValue);
      assertTrue(identifier.isOfThisType(maskedValue));
      if (!maskedValue.equals(originalValue)) {
        randomizationOK++;
      }
      // System.out.println("=======> Original value male, masked value ["
      // + maskedValue + "]");
    }

    assertTrue(randomizationOK > 0);

    originalValue = "Female";
    randomizationOK = 0;

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(originalValue);
      assertTrue(identifier.isOfThisType(maskedValue));
      if (!maskedValue.equals(originalValue)) {
        randomizationOK++;
      }

      // System.out.println("=======> Original value Female, masked value
      // ["
      // + maskedValue + "]");
    }

    assertTrue(randomizationOK > 0);

    originalValue = "Both";
    randomizationOK = 0;

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(originalValue);
      assertEquals(null, maskedValue);
      if (maskedValue != null) {
        randomizationOK++;
      }

      // System.out.println("=======> Origianl value Both, masked value ["
      // + maskedValue + "]");
    }

    assertTrue(randomizationOK == 0);
  }

  @Test
  public void testMaskNullGenderInputReturnNull() throws Exception {
    GenderMaskingProviderConfig maskingConfiguration = new GenderMaskingProviderConfig();
    MaskingProvider maskingProvider = new GenderMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String invalidGender = null;
    String maskedGender = maskingProvider.mask(invalidGender);

    assertEquals(null, maskedGender);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidGenderInputValidHandlingReturnNull() throws Exception {
    GenderMaskingProviderConfig maskingConfiguration = new GenderMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider = new GenderMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String invalidGender = "Invalid Gender";
    String maskedGender = maskingProvider.mask(invalidGender);

    assertEquals(null, maskedGender);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidGenderInputValidHandlingReturnRandom() throws Exception {
    GenderMaskingProviderConfig maskingConfiguration = new GenderMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider = new GenderMaskingProvider(maskingConfiguration, tenantId, localizationProperty);
    Identifier identifier = new GenderIdentifier();

    String invalidGender = "Invalid Gender";
    String maskedGender = maskingProvider.mask(invalidGender);

    assertFalse(maskedGender.equals(invalidGender));
    assertTrue(identifier.isOfThisType(maskedGender));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidGenderInputValidHandlingReturnDefaultCustomValue() throws Exception {
    GenderMaskingProviderConfig maskingConfiguration = new GenderMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new GenderMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String invalidGender = "Invalid Gender";
    String maskedGender = maskingProvider.mask(invalidGender);

    assertEquals("OTHER", maskedGender);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidGenderInputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    GenderMaskingProviderConfig maskingConfiguration = new GenderMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(3);
    maskingConfiguration.setUnspecifiedValueReturnMessage("Test Gender");
    MaskingProvider maskingProvider = new GenderMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String invalidGender = "Invalid Gender";
    String maskedGender = maskingProvider.mask(invalidGender);

    assertEquals("Test Gender", maskedGender);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidGenderInputInvalidHandlingReturnNull() throws Exception {
    GenderMaskingProviderConfig maskingConfiguration = new GenderMaskingProviderConfig();
    maskingConfiguration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new GenderMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    String invalidGender = "Invalid Gender";
    String maskedGender = maskingProvider.mask(invalidGender);

    assertEquals(null, maskedGender);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

}
