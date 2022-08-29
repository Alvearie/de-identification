/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.SWIFTMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;

public class SWIFTCodeMaskingProviderTest extends SWIFTCodeMaskingProviderTestSetup {

  @Test
  public void testNoPreserve() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    SWIFTCodeMaskingProvider maskingProvider =
        new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    checkRandomGenerated("EMCRGRA1X", maskingProvider);
    checkRandomGenerated("EMCRGRA1XX", maskingProvider);
    checkRandomGenerated("EMCRGRA1XXX", maskingProvider);
    checkRandomGenerated("EMCR", maskingProvider);
    checkRandomGenerated("", maskingProvider);
    checkRandomGenerated("12345678", maskingProvider);
    checkRandomGenerated("&&&", maskingProvider);
    checkRandomGenerated("&&&&&&&&", maskingProvider);
    checkRandomGenerated("aaaaaaaa", maskingProvider);
  }

  @Test
  public void testPreserveCountry_null() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    maskingConfiguration.setPreserveCountry(true);
    maskingConfiguration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    SWIFTCodeMaskingProvider maskingProvider =
        new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    assertNull(maskingProvider.mask("EMCRGRA1X"));
    assertNull(maskingProvider.mask("EMCRGRA1XX"));
    checkRandomGenerated("EMCRGRA1XXX", maskingProvider, "GR");
    assertNull(maskingProvider.mask("EMCR"));
    assertNull(maskingProvider.mask(""));
    assertNull(maskingProvider.mask(null));
    assertNull(maskingProvider.mask("12345678"));
    assertNull(maskingProvider.mask("&&&"));
    assertNull(maskingProvider.mask("&&&&&&&&"));
    checkRandomGenerated("aaaaaaaa", maskingProvider, "AA");
  }

  @Test
  public void testPreserveCountry_random() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    maskingConfiguration.setPreserveCountry(true);
    maskingConfiguration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    SWIFTCodeMaskingProvider maskingProvider =
        new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    checkRandomGenerated("EMCRGRA1x", maskingProvider);
    checkRandomGenerated("EMCRGRA1xx", maskingProvider);
    checkRandomGenerated("EMCRGRA1XXX", maskingProvider, "GR");
    checkRandomGenerated("EMCR", maskingProvider);
    checkRandomGenerated("", maskingProvider);
    checkRandomGenerated("12345678", maskingProvider);
    checkRandomGenerated("&&&", maskingProvider);
    checkRandomGenerated("&&&&&&&&", maskingProvider);
    checkRandomGenerated("aaaaaaaa", maskingProvider, "AA");
  }

  @Test
  public void testPreserveCountry_other() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    maskingConfiguration.setPreserveCountry(true);
    maskingConfiguration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    maskingConfiguration.setUnexpectedInputReturnMessage("slow");
    SWIFTCodeMaskingProvider maskingProvider =
        new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    assertEquals("slow", maskingProvider.mask("EMCRGRA12"));
    assertEquals("slow", maskingProvider.mask("EMCRGRA122"));
    checkRandomGenerated("EMCRGRA1222", maskingProvider, "GR");
    assertEquals("slow", maskingProvider.mask("EMCR"));
    assertEquals("slow", maskingProvider.mask(""));
    assertEquals("slow", maskingProvider.mask("12345678"));
    assertEquals("slow", maskingProvider.mask("&&&"));
    assertEquals("slow", maskingProvider.mask("&&&&&&&&"));
    checkRandomGenerated("aaaaaaaa", maskingProvider, "AA");
  }

  @Test
  public void testValuesLoaded_NoPreserve() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    maskingConfiguration.setPreserveCountry(false);
    SWIFTCodeMaskingProvider maskingProvider =
        new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId, TEST_LOCALIZATION_PROPERTIES);

    checkOneOf("EMCRGRA1X", maskingProvider, REPLACEMENTS);
    checkOneOf("EMCRGRA1XX", maskingProvider, REPLACEMENTS);
    checkOneOf("EMCRGRA1XXX", maskingProvider, REPLACEMENTS);
    checkOneOf("EMCR", maskingProvider, REPLACEMENTS);
    checkOneOf("", maskingProvider, REPLACEMENTS);
    checkOneOf("12345678", maskingProvider, REPLACEMENTS);
    checkOneOf("&&&", maskingProvider, REPLACEMENTS);
    checkOneOf("&&&&&&&&", maskingProvider, REPLACEMENTS);
    checkOneOf("aaaaaaaa", maskingProvider, REPLACEMENTS);
    checkOneOf("nnnnUSxx", maskingProvider, REPLACEMENTS);
    checkOneOf("nnnnCAxx", maskingProvider, REPLACEMENTS);
  }

  @Test
  public void testValuesLoaded_null() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    maskingConfiguration.setPreserveCountry(true);
    // allow unspecified value handling to default
    SWIFTCodeMaskingProvider maskingProvider =
        new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId, TEST_LOCALIZATION_PROPERTIES);

    // valid format, but country not loaded
    assertNull(maskingProvider.mask("EMCRGRA1X"));
    assertNull(maskingProvider.mask("EMCRGRA1XX"));
    checkOneOf("EMCRGRA1XXX", maskingProvider, REPLACEMENTS);
    assertNull(maskingProvider.mask("EMCR"));
    assertNull(maskingProvider.mask(""));
    assertNull(maskingProvider.mask(null));
    assertNull(maskingProvider.mask("12345678"));
    assertNull(maskingProvider.mask("&&&"));
    assertNull(maskingProvider.mask("&&&&&&&&"));
    checkOneOf("aaaaaaaa", maskingProvider, REPLACEMENTS);
    checkOneOf("nnnnUSxx", maskingProvider, US_REPLACEMENTS);
    checkOneOf("nnnnCAxx", maskingProvider, CA_REPLACEMENTS);
  }

  @Test
  public void testValuesLoaded_random() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    maskingConfiguration.setPreserveCountry(true);
    maskingConfiguration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    SWIFTCodeMaskingProvider maskingProvider =
        new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId, TEST_LOCALIZATION_PROPERTIES);

    checkOneOf("EMCRGRA1X", maskingProvider, REPLACEMENTS);
    checkOneOf("EMCRGRA1XX", maskingProvider, REPLACEMENTS);
    checkOneOf("EMCRGRA1XXX", maskingProvider, REPLACEMENTS);
    checkOneOf("EMCR", maskingProvider, REPLACEMENTS);
    checkOneOf("", maskingProvider, REPLACEMENTS);
    checkOneOf("12345678", maskingProvider, REPLACEMENTS);
    checkOneOf("&&&", maskingProvider, REPLACEMENTS);
    checkOneOf("&&&&&&&&", maskingProvider, REPLACEMENTS);
    checkOneOf("aaaaaaaa", maskingProvider, REPLACEMENTS);
    checkOneOf("nnnnUSxx", maskingProvider, US_REPLACEMENTS);
    checkOneOf("nnnnCAxx", maskingProvider, CA_REPLACEMENTS);
  }

  @Test
  public void testValuesLoaded_other() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    maskingConfiguration.setPreserveCountry(true);
    maskingConfiguration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    // allow message to default
    SWIFTCodeMaskingProvider maskingProvider =
        new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId, TEST_LOCALIZATION_PROPERTIES);

    // valid format, but country not loaded
    assertEquals("OTHER", maskingProvider.mask("EMCRGRA1X"));
    assertEquals("OTHER", maskingProvider.mask("EMCRGRA1XX"));
    checkOneOf("EMCRGRA1XXX", maskingProvider, REPLACEMENTS);
    assertEquals("OTHER", maskingProvider.mask("EMCR"));
    assertEquals("OTHER", maskingProvider.mask(""));
    assertEquals("OTHER", maskingProvider.mask("12345678"));
    assertEquals("OTHER", maskingProvider.mask("&&&"));
    assertEquals("OTHER", maskingProvider.mask("&&&&&&&&"));
    checkOneOf("aaaaaaaa", maskingProvider, REPLACEMENTS);
    checkOneOf("nnnnUSxx", maskingProvider, US_REPLACEMENTS);
    checkOneOf("nnnnCAxx", maskingProvider, CA_REPLACEMENTS);
  }
}
