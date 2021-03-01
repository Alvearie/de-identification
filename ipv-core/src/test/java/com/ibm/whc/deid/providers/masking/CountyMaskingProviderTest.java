/*
 * (C) Copyright IBM Corp. 2016,2021
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

import com.ibm.whc.deid.providers.identifiers.CountyIdentifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.shared.pojo.config.masking.CountyMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;

public class CountyMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {
  /*
   * Test for pseudorandom boolean option (true and false).
   */
  @Test
  public void testPseudorandom() throws Exception {
    CountyMaskingProviderConfig configuration = new CountyMaskingProviderConfig();
    assertEquals(MaskingProviderType.COUNTY, configuration.getType());
    configuration.setMaskPseudorandom(true);

    MaskingProvider maskingProvider =
        new CountyMaskingProvider(configuration, tenantId, localizationProperty);

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
  public void testMask() {
    // county.mask.pseudorandom by default is off
    Identifier identifier = new CountyIdentifier(tenantId, localizationProperty);
    CountyMaskingProviderConfig configuration = new CountyMaskingProviderConfig();
    assertEquals(MaskingProviderType.COUNTY, configuration.getType());

    MaskingProvider maskingProvider =
        new CountyMaskingProvider(configuration, tenantId, localizationProperty);

    String originalValue = "Pendleton County";
    assertTrue(identifier.isOfThisType(originalValue));

    int randomizationOK = 0;

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(originalValue);
      assertTrue(identifier.isOfThisType(maskedValue));

      if (!maskedValue.equals(originalValue)) {
        randomizationOK++;
      }
    }

    assertTrue(randomizationOK > 0);
  }

  @Test
  public void testMaskNullCountyInputReturnNull() throws Exception {
    CountyMaskingProviderConfig configuration = new CountyMaskingProviderConfig();

    MaskingProvider maskingProvider =
        new CountyMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidCounty = null;
    String maskedCounty = maskingProvider.mask(invalidCounty);

    assertEquals(null, maskedCounty);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCountyInputValidHandlingReturnNull() throws Exception {
    CountyMaskingProviderConfig configuration = new CountyMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider =
        new CountyMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidCounty = "Invalid County";
    String maskedCounty = maskingProvider.mask(invalidCounty);

    assertEquals(null, maskedCounty);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCountyInputValidHandlingReturnRandom() throws Exception {
    CountyMaskingProviderConfig configuration = new CountyMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider =
        new CountyMaskingProvider(configuration, tenantId, localizationProperty);
    Identifier identifier = new CountyIdentifier(tenantId, localizationProperty);

    String invalidCounty = "Invalid County";
    String maskedCounty = maskingProvider.mask(invalidCounty);

    assertFalse(maskedCounty.equals(invalidCounty));
    assertTrue(identifier.isOfThisType(maskedCounty));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCountyInputValidHandlingReturnDefaultCustomValue() throws Exception {
    CountyMaskingProviderConfig configuration = new CountyMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider =
        new CountyMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidCounty = "Invalid County";
    String maskedCounty = maskingProvider.mask(invalidCounty);

    assertEquals("OTHER", maskedCounty);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCountyInputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    CountyMaskingProviderConfig configuration = new CountyMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("Test County");
    MaskingProvider maskingProvider =
        new CountyMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidCounty = "Invalid County";
    String maskedCounty = maskingProvider.mask(invalidCounty);

    assertEquals("Test County", maskedCounty);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCountyInputInvalidHandlingReturnNull() throws Exception {
    CountyMaskingProviderConfig configuration = new CountyMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider =
        new CountyMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidCounty = "Invalid County";
    String maskedCounty = maskingProvider.mask(invalidCounty);

    assertEquals(null, maskedCounty);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

}
