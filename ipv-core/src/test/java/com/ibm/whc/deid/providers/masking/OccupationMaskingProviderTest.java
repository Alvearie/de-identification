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
import static org.junit.Assert.fail;
import java.util.Arrays;
import java.util.List;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.OccupationIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.OccupationMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;

public class OccupationMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  /*
   * Tests mask generalize to occupation category and its boolean values (true and false). It also
   * tests random occupation generation.
   */

  @Test
  public void testMaskRandomOccupation() {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    MaskingProvider maskingProvider =
        new OccupationMaskingProvider(configuration, tenantId, localizationProperty);
    Identifier identifier = new OccupationIdentifier(tenantId, localizationProperty);

    String occupation = "actor";
    boolean changed = false;
    for (int i = 0; i < 20; i++) {
      String maskedValue = maskingProvider.mask(occupation);
      assertTrue(maskedValue, identifier.isOfThisType(maskedValue));
      if (!occupation.equalsIgnoreCase(maskedValue)) {
        changed = true;
      }
    }
    assertTrue(changed);

    occupation = "xxxxx";
    for (int i = 0; i < 20; i++) {
      String maskedValue = maskingProvider.mask(occupation);
      assertTrue(maskedValue, identifier.isOfThisType(maskedValue));
    }

    occupation = "";
    for (int i = 0; i < 20; i++) {
      String maskedValue = maskingProvider.mask(occupation);
      assertTrue(maskedValue, identifier.isOfThisType(maskedValue));
    }
  }

  @Test
  public void testMaskInvalidOccupationInputValidHandlingReturnNull() throws Exception {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    configuration.setMaskGeneralize(true);
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
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
    configuration.setMaskGeneralize(true);
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    MaskingProvider maskingProvider =
        new OccupationMaskingProvider(configuration, tenantId, localizationProperty);
    Identifier identifier = new OccupationIdentifier(tenantId, localizationProperty);

    String invalidOccupation = "Invalid Occupation";
    String maskedOccupation = maskingProvider.mask(invalidOccupation);

    assertTrue(identifier.isOfThisType(maskedOccupation));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidOccupationInputValidHandlingReturnDefaultCustomValue()
      throws Exception {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    configuration.setMaskGeneralize(true);
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
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
    configuration.setMaskGeneralize(true);
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    configuration.setUnexpectedInputReturnMessage("Test Occupation");
    MaskingProvider maskingProvider =
        new OccupationMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidOccupation = "Invalid Occupation";
    String maskedOccupation = maskingProvider.mask(invalidOccupation);

    assertEquals("Test Occupation", maskedOccupation);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidOccupationInputValidHandlingExit() throws Exception {
    OccupationMaskingProviderConfig configuration = new OccupationMaskingProviderConfig();
    configuration.setMaskGeneralize(true);
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.ERROR_EXIT);
    MaskingProvider maskingProvider =
        new OccupationMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidOccupation = "Invalid Occupation";
    try {
      maskingProvider.mask(invalidOccupation);
      fail("expected exception");
    } catch (PrivacyProviderInvalidInputException e) {
      assertFalse(e.getMessage().contains(invalidOccupation));
    }
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

    List<String> possibles = Arrays.asList("Artists", "Actors, entertainers and presenters",
        "Glass and ceramics makers, decorators and finishers", "Other skilled trades n.e.c.");
    occupation = "ARTIST";
    for (int i = 0; i < 20; i++) {
      maskedValue = maskingProvider.mask(occupation);
      assertTrue(maskedValue, possibles.contains(maskedValue));
    }
  }
}
