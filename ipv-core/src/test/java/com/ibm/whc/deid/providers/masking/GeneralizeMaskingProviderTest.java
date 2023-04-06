/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.shared.pojo.config.masking.GeneralizeMaskingProviderConfig;
import org.junit.Test;

public class GeneralizeMaskingProviderTest extends TestLogSetUp {

  @Test
  public void testSourceIn() {
    String maskingOptionValue =
        "[{\"targetValue\": \"AsiaCity\", \"sourceValueIn\": [\"Bangkok\",\"Manila\",\"Shanghi\",\"TiPei\",\"Mumbai\"]},"
            + " {\"targetValue\": \"AfricaCity\", \"sourceValueIn\": [\"Addis Ababa\",\"Cairo\",\"Cape Town\",\"Lagos\"]},"
            + " {\"targetValue\": null, \"sourceValueIn\": [\"Rejected City\"]}]";

    GeneralizeMaskingProviderConfig configuration = new GeneralizeMaskingProviderConfig();
    configuration.setMaskRuleSet(maskingOptionValue);

    GeneralizeMaskingProvider maskingProvider = new GeneralizeMaskingProvider(configuration);

    assertEquals("AsiaCity", maskingProvider.mask("Bangkok"));
    assertEquals("AsiaCity", maskingProvider.mask("Manila"));
    assertEquals("AsiaCity", maskingProvider.mask("Shanghi"));
    assertEquals("AsiaCity", maskingProvider.mask("TiPei"));
    assertEquals("AsiaCity", maskingProvider.mask("Mumbai"));

    assertEquals("AfricaCity", maskingProvider.mask("Addis Ababa"));
    assertEquals("AfricaCity", maskingProvider.mask("Cairo"));
    assertEquals("AfricaCity", maskingProvider.mask("Cape Town"));
    assertEquals("AfricaCity", maskingProvider.mask("Lagos"));

    assertNull(maskingProvider.mask("Rejected City"));

    assertEquals("Rochester", maskingProvider.mask("Rochester"));
    assertEquals("not listed city", maskingProvider.mask("not listed city"));
  }

  @Test
  public void testSourceInWildcard() {
    String maskingOptionValue =
        "[{\"targetValue\": \"USCity\", \"sourceValueIn\": [\"New York\",\"Chicago\",\"Houston\",\"Minneapolis\",\"Boston\" ]},"
            + "{\"targetValue\": \"CanadaCity\", \"sourceValueIn\": [\"Toronto\",\"Montreal\",\"Vancouver\",\"Calgary\"]},"
            + "{\"targetValue\": \"Other\", \"sourceValueIn\": [\"*\"]}]";

    GeneralizeMaskingProviderConfig configuration = new GeneralizeMaskingProviderConfig();
    configuration.setMaskRuleSet(maskingOptionValue);

    GeneralizeMaskingProvider maskingProvider = new GeneralizeMaskingProvider(configuration);

    assertEquals("USCity", maskingProvider.mask("Chicago"));
    assertEquals("CanadaCity", maskingProvider.mask("Montreal"));
    assertEquals("Other", maskingProvider.mask("St. Paul"));
  }

  @Test
  public void testSourceNotIn() {
    String maskingOptionValue =
        "[{\"targetValue\": \"Other\", \"sourceValueNotIn\": [\"French\",\"Spanish\"]}]";

    GeneralizeMaskingProviderConfig configuration = new GeneralizeMaskingProviderConfig();
    configuration.setMaskRuleSet(maskingOptionValue);

    GeneralizeMaskingProvider maskingProvider = new GeneralizeMaskingProvider(configuration);

    assertEquals("Spanish", maskingProvider.mask("Spanish"));
    assertEquals("Other", maskingProvider.mask("German"));
    assertEquals("Other", maskingProvider.mask("spanish")); // case-sensitivie
    assertEquals("French", maskingProvider.mask("French"));
  }

  @Test
  public void testMaskNullGeneralizeInputReturnNull() throws Exception {
    GeneralizeMaskingProviderConfig configuration = new GeneralizeMaskingProviderConfig();
    MaskingProvider maskingProvider = new GeneralizeMaskingProvider(configuration);

    String invalidGeneralizeInput = null;
    String maskedValue = maskingProvider.mask(invalidGeneralizeInput);

    assertEquals(null, maskedValue);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testTargetValueParsingError() {
    try {
      String maskingOptionValue =
          "[{\"targetValuexxx\": \"Other\", \"sourceValueIn\": [\"Italian\",\"English\"], \"sourceValueNotIn\": [\"French\",\"Spanish\"]}]";
      GeneralizeMaskingProviderConfig configuration = new GeneralizeMaskingProviderConfig();
      configuration.setMaskRuleSet(maskingOptionValue);

      new GeneralizeMaskingProvider(configuration);
    } catch (RuntimeException re) {
      assertTrue(re.getMessage().contains("ruleSet"));
      return;
    }
    assertTrue(false);
  }

  @Test
  public void testEmptyConfig() {
    GeneralizeMaskingProvider maskingProvider =
        new GeneralizeMaskingProvider(new GeneralizeMaskingProviderConfig());
    assertNull(maskingProvider.mask("value1"));
  }

  @Test
  public void testConflictSourceValueParsingError() {
    try {
      // Currently, must specify either sourceValueIn or sourceValueNotIn
      // but not both.
      String maskingOptionValue =
          "[{\"targetValue\": \"Other\", \"sourceValueIn\": [\"Italian\",\"English\"], \"sourceValueNotIn\": [\"French\",\"Spanish\"]}]";

      GeneralizeMaskingProviderConfig configuration = new GeneralizeMaskingProviderConfig();
      configuration.setMaskRuleSet(maskingOptionValue);

      new GeneralizeMaskingProvider(configuration);
    } catch (RuntimeException re) {
      assertTrue(re.getMessage().contains("ruleSet"));
      return;
    }
    assertTrue(false);
  }

  @Test
  public void testRuntimeException() {
    try {
      String maskingOptionValue =
          "[[[[[[\"bad format \": \"Other\", \"sourceValueNotIn\": [\"French\",\"Spanish\"]}]";

      GeneralizeMaskingProviderConfig configuration = new GeneralizeMaskingProviderConfig();
      configuration.setMaskRuleSet(maskingOptionValue);

      new GeneralizeMaskingProvider(configuration);
    } catch (RuntimeException re) {
      assertTrue(re.getMessage().contains("ruleSet"));
      return;
    }
    assertTrue(false);
  }
}
