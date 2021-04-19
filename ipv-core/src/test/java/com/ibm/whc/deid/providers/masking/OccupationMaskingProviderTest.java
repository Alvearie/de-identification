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

import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.OccupationIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.OccupationMaskingProviderConfig;

public class OccupationMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  /*
   * Tests mask generalize to occupation category and its boolean values (true and false). It also
   * tests for an invalid value and the localization of the occupation.
   */
  @Test
  public void testMaskRandomOccupation() {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    MaskingProvider maskingProvider =
        new OccupationMaskingProvider(configuration, tenantId, localizationProperty);
    Identifier identifier = new OccupationIdentifier(tenantId, localizationProperty);

    String occupation = "actor";
    String maskedValue = maskingProvider.mask(occupation);
    assertTrue(identifier.isOfThisType(maskedValue));
  }

  @Test
  public void testMaskNullOccupationInputReturnNull() throws Exception {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    MaskingProvider maskingProvider =
        new OccupationMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidOccupation = null;
    String maskedOccupation = maskingProvider.mask(invalidOccupation);

    assertEquals(null, maskedOccupation);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidOccupationInputValidHandlingReturnNull() throws Exception {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider =
        new OccupationMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidOccupation = "Invalid Occupation";
    String maskedOccupation = maskingProvider.mask(invalidOccupation);

    assertEquals(null, maskedOccupation);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidOccupationInputValidHandlingReturnRandom() throws Exception {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider =
        new OccupationMaskingProvider(configuration, tenantId, localizationProperty);
    Identifier identifier = new OccupationIdentifier(tenantId, localizationProperty);

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
    MaskingProvider maskingProvider =
        new OccupationMaskingProvider(configuration, tenantId, localizationProperty);

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
    MaskingProvider maskingProvider =
        new OccupationMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidOccupation = "Invalid Occupation";
    String maskedOccupation = maskingProvider.mask(invalidOccupation);

    assertEquals("Test Occupation", maskedOccupation);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidOccupationInputInvalidHandlingReturnNull() throws Exception {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider =
        new OccupationMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidOccupation = "Invalid Occupation";
    String maskedOccupation = maskingProvider.mask(invalidOccupation);

    assertEquals(null, maskedOccupation);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskGeneralizeToCategory() {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    configuration.setMaskGeneralize(true);
    MaskingProvider maskingProvider =
        new OccupationMaskingProvider(configuration, tenantId, localizationProperty);

    String occupation = "actor";
    String maskedValue = maskingProvider.mask(occupation);
    assertTrue(maskedValue.equals("Actors, entertainers and presenters"));
  }
}
