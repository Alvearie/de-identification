/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;

public class SWIFTCodeMaskingProviderTestSetup extends TestLogSetUp implements MaskingProviderTest {

  protected static final String TEST_LOCALIZATION_PROPERTIES =
      "/localization/test.swift.localization.properties";

  // values from the TEST_LOCALIZATION_PROPERTIES file
  protected static final String[] REPLACEMENTS = {"AAAACAAA", "BBBBCABB", "CCCCCACC", "DDDDCADD",
      "EEEEUSEE", "FFFFUSFF", "GGGGUSGG", "HHHHUSHH"};
  protected static final String[] CA_REPLACEMENTS =
      {"AAAACAAA", "BBBBCABB", "CCCCCACC", "DDDDCADD"};
  protected static final String[] US_REPLACEMENTS =
      {"EEEEUSEE", "FFFFUSFF", "GGGGUSGG", "HHHHUSHH"};

  protected String checkRandomGenerated(String original, SWIFTCodeMaskingProvider provider) {
    String value = provider.mask(original);
    assertNotNull(value);
    assertTrue(
        value + " fails to match pattern " + SWIFTCodeMaskingProvider.SWIFTCODE_PATTERN.pattern(),
        SWIFTCodeMaskingProvider.SWIFTCODE_PATTERN.matcher(value).matches());
    assertNotEquals(original, value);
    return value;
  }

  protected String checkOneOf(String original, SWIFTCodeMaskingProvider provider,
      String... possibles) {
    String value = provider.mask(original);
    assertTrue("unexpected value " + value, Arrays.asList(possibles).contains(value));
    assertNotEquals(original, value);
    return value;
  }
}
