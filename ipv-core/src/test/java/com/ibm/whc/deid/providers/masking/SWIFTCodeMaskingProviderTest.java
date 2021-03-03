/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.SWIFTMaskingProviderConfig;

public class SWIFTCodeMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  private static final String TEST_LOCALIZATION_PROPERTIES =
      "/localization/test.swift.localization.properties";

  // values from the TEST_LOCALIZATION_PROPERTIES file
  private static final String[] REPLACEMENTS = {"AAAACAAA", "BBBBCABB", "CCCCCACC", "DDDDCADD",
      "EEEEUSEE", "FFFFUSFF", "GGGGUSGG", "HHHHUSHH"};

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
    checkRandomGenerated(null, maskingProvider);
    checkRandomGenerated("12345678", maskingProvider);
    checkRandomGenerated("&&&", maskingProvider);
    checkRandomGenerated("&&&&&&&&", maskingProvider);
    checkRandomGenerated("aaaaaaaa", maskingProvider);
  }

  @Test
  public void testPreserveCountry_null() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    maskingConfiguration.setPreserveCountry(true);
    maskingConfiguration.setUnspecifiedValueHandling(1);
    SWIFTCodeMaskingProvider maskingProvider =
        new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    assertNull(maskingProvider.mask("EMCRGRA1X"));
    assertNull(maskingProvider.mask("EMCRGRA1XX"));
    String masked = checkRandomGenerated("EMCRGRA1XXX", maskingProvider);
    assertEquals("GR", masked.substring(4, 6));
    assertNull(maskingProvider.mask("EMCR"));
    assertNull(maskingProvider.mask(""));
    assertNull(maskingProvider.mask(null));
    assertNull(maskingProvider.mask("12345678"));
    assertNull(maskingProvider.mask("&&&"));
    assertNull(maskingProvider.mask("&&&&&&&&"));
    masked = checkRandomGenerated("aaaaaaaa", maskingProvider);
    assertEquals("AA", masked.substring(4, 6));
  }

  @Test
  public void testPreserveCountry_random() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    maskingConfiguration.setPreserveCountry(true);
    maskingConfiguration.setUnspecifiedValueHandling(2);
    SWIFTCodeMaskingProvider maskingProvider =
        new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    checkRandomGenerated("EMCRGRA1x", maskingProvider);
    checkRandomGenerated("EMCRGRA1xx", maskingProvider);
    String masked = checkRandomGenerated("EMCRGRA1XXX", maskingProvider);
    assertEquals("GR", masked.substring(4, 6));
    checkRandomGenerated("EMCR", maskingProvider);
    checkRandomGenerated("", maskingProvider);
    checkRandomGenerated(null, maskingProvider);
    checkRandomGenerated("12345678", maskingProvider);
    checkRandomGenerated("&&&", maskingProvider);
    checkRandomGenerated("&&&&&&&&", maskingProvider);
    masked = checkRandomGenerated("aaaaaaaa", maskingProvider);
    assertEquals("AA", masked.substring(4, 6));
  }

  @Test
  public void testPreserveCountry_other() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    maskingConfiguration.setPreserveCountry(true);
    maskingConfiguration.setUnspecifiedValueHandling(3);
    maskingConfiguration.setUnspecifiedValueReturnMessage("slow");
    SWIFTCodeMaskingProvider maskingProvider =
        new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

    assertEquals("slow", maskingProvider.mask("EMCRGRA12"));
    assertEquals("slow", maskingProvider.mask("EMCRGRA122"));
    String masked = checkRandomGenerated("EMCRGRA1222", maskingProvider);
    assertEquals("GR", masked.substring(4, 6));
    assertEquals("slow", maskingProvider.mask("EMCR"));
    assertEquals("slow", maskingProvider.mask(""));
    assertEquals("slow", maskingProvider.mask(null));
    assertEquals("slow", maskingProvider.mask("12345678"));
    assertEquals("slow", maskingProvider.mask("&&&"));
    assertEquals("slow", maskingProvider.mask("&&&&&&&&"));
    masked = checkRandomGenerated("aaaaaaaa", maskingProvider);
    assertEquals("AA", masked.substring(4, 6));
  }

  private String checkRandomGenerated(String original, SWIFTCodeMaskingProvider provider) {
    String value = provider.mask(original);
    assertNotNull(value);
    assertTrue(
        value + " fails to match pattern " + SWIFTCodeMaskingProvider.SWIFTCODE_PATTERN.pattern(),
        SWIFTCodeMaskingProvider.SWIFTCODE_PATTERN.matcher(value).matches());
    assertNotEquals(original, value);
    return value;
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
    checkOneOf(null, maskingProvider, REPLACEMENTS);
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
    checkOneOf("nnnnUSxx", maskingProvider, "EEEEUSEE", "FFFFUSFF", "GGGGUSGG", "HHHHUSHH");
    checkOneOf("nnnnCAxx", maskingProvider, "AAAACAAA", "BBBBCABB", "CCCCCACC", "DDDDCADD");
  }

  @Test
  public void testValuesLoaded_random() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    maskingConfiguration.setPreserveCountry(true);
    maskingConfiguration.setUnspecifiedValueHandling(2);
    SWIFTCodeMaskingProvider maskingProvider =
        new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId, TEST_LOCALIZATION_PROPERTIES);

    checkOneOf("EMCRGRA1X", maskingProvider, REPLACEMENTS);
    checkOneOf("EMCRGRA1XX", maskingProvider, REPLACEMENTS);
    checkOneOf("EMCRGRA1XXX", maskingProvider, REPLACEMENTS);
    checkOneOf("EMCR", maskingProvider, REPLACEMENTS);
    checkOneOf("", maskingProvider, REPLACEMENTS);
    checkOneOf(null, maskingProvider, REPLACEMENTS);
    checkOneOf("12345678", maskingProvider, REPLACEMENTS);
    checkOneOf("&&&", maskingProvider, REPLACEMENTS);
    checkOneOf("&&&&&&&&", maskingProvider, REPLACEMENTS);
    checkOneOf("aaaaaaaa", maskingProvider, REPLACEMENTS);
    checkOneOf("nnnnUSxx", maskingProvider, "EEEEUSEE", "FFFFUSFF", "GGGGUSGG", "HHHHUSHH");
    checkOneOf("nnnnCAxx", maskingProvider, "AAAACAAA", "BBBBCABB", "CCCCCACC", "DDDDCADD");
  }

  @Test
  public void testValuesLoaded_other() {
    SWIFTMaskingProviderConfig maskingConfiguration = new SWIFTMaskingProviderConfig();
    maskingConfiguration.setPreserveCountry(true);
    maskingConfiguration.setUnspecifiedValueHandling(3);
    // allow message to default
    SWIFTCodeMaskingProvider maskingProvider =
        new SWIFTCodeMaskingProvider(maskingConfiguration, tenantId, TEST_LOCALIZATION_PROPERTIES);

    // valid format, but country not loaded
    assertEquals("OTHER", maskingProvider.mask("EMCRGRA1X"));
    assertEquals("OTHER", maskingProvider.mask("EMCRGRA1XX"));
    checkOneOf("EMCRGRA1XXX", maskingProvider, REPLACEMENTS);
    assertEquals("OTHER", maskingProvider.mask("EMCR"));
    assertEquals("OTHER", maskingProvider.mask(""));
    assertEquals("OTHER", maskingProvider.mask(null));
    assertEquals("OTHER", maskingProvider.mask("12345678"));
    assertEquals("OTHER", maskingProvider.mask("&&&"));
    assertEquals("OTHER", maskingProvider.mask("&&&&&&&&"));
    checkOneOf("aaaaaaaa", maskingProvider, REPLACEMENTS);
    checkOneOf("nnnnUSxx", maskingProvider, "EEEEUSEE", "FFFFUSFF", "GGGGUSGG", "HHHHUSHH");
    checkOneOf("nnnnCAxx", maskingProvider, "AAAACAAA", "BBBBCABB", "CCCCCACC", "DDDDCADD");
  }

  private String checkOneOf(String original, SWIFTCodeMaskingProvider provider,
      String... possibles) {
    String value = provider.mask(original);
    assertTrue("unexpected value " + value, Arrays.asList(possibles).contains(value));
    assertNotEquals(original, value);
    return value;
  }
}
