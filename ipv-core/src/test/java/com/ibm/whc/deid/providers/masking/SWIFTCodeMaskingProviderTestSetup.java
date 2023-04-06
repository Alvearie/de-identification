/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import java.util.List;
import com.ibm.whc.deid.models.SWIFTCode;

public class SWIFTCodeMaskingProviderTestSetup extends TestLogSetUp implements MaskingProviderTest {

  // values from the TEST_LOCALIZATION_PROPERTIES file
  protected static final String[] REPLACEMENTS = {"AAAACA11", "BBBBCABB", "CCCCCACC", "DDDDCADD004",
      "EEEEUS55XX5", "FFFFUSFF", "GGGGUSGG", "HHHHUSHH", "IIIIXXII"};
  protected static final String[] CA_REPLACEMENTS =
      {"AAAACA11", "BBBBCABB", "CCCCCACC", "DDDDCADD004"};
  protected static final String[] US_REPLACEMENTS =
      {"EEEEUS55XX5", "FFFFUSFF", "GGGGUSGG", "HHHHUSHH"};
  protected static final List<String> REPLACEMENTS_LIST = Arrays.asList(REPLACEMENTS);
  protected static final List<String> CA_REPLACEMENTS_LIST = Arrays.asList(CA_REPLACEMENTS);
  protected static final List<String> US_REPLACEMENTS_LIST = Arrays.asList(US_REPLACEMENTS);

  protected void checkRandomGenerated(String original, SWIFTCodeMaskingProvider provider) {
    checkRandomGenerated(original, provider, null);
  }

  protected void checkRandomGenerated(String original, SWIFTCodeMaskingProvider provider,
      String countryCode) {
    for (int i = 0; i < 20; i++) {
      String value = provider.mask(original);
      assertNotNull(value);
      assertTrue(
          value + " fails to match pattern " + SWIFTCode.SWIFTCODE_PATTERN.pattern(),
          SWIFTCode.SWIFTCODE_PATTERN.matcher(value).matches());
      assertNotEquals(original, value);
      if (countryCode != null) {
        assertEquals(countryCode, value.substring(4, 6));
      }
    }
  }

  protected void checkOneOf(String original, SWIFTCodeMaskingProvider provider,
      String... possibles) {
    for (int i = 0; i < 20; i++) {
      String value = provider.mask(original);
      assertTrue("unexpected value " + value, Arrays.asList(possibles).contains(value));
      assertNotEquals(original, value);
    }
  }
}
