/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.shared.pojo.config.masking.GeneralizeMaskingProviderConfig;
import org.junit.Test;

public class GeneralizeMaskingProviderTest extends TestLogSetUp {

  @Test
  public void testPatientCityReplaced() {
    String maskingOptionValue =
        "[{\"targetValue\": \"AsiaCity\", \"sourceValueIn\": [\"Bangkok\",\"Manila\",\"Shanghi\",\"TiPei\",\"Mumbai\"]},"
            + " {\"targetValue\": \"AfricaCity\", \"sourceValueIn\": [\"Addis Ababa\",\"Cairo\",\"Cape Town\",\"Lagos\"]}]";

    GeneralizeMaskingProviderConfig configuration = new GeneralizeMaskingProviderConfig();
    configuration.setMaskRuleSet(maskingOptionValue);

    GeneralizeMaskingProvider maskingProvider = new GeneralizeMaskingProvider(configuration);

    String identifier = "Bangkok";
    String maskedValue = maskingProvider.mask(identifier);

    // The identifier "Bangkok" should be generalized to "AsiaCity" since it
    // exists in AsiaCity city values
    assertTrue(maskedValue.equals("AsiaCity"));
  }

  @Test
  public void testPatientCityNotReplaced() {
    String maskingOptionValue =
        "[{\"targetValue\": \"AsiaCity\", \"sourceValueIn\": [\"Bangkok\",\"Manila\",\"Shanghi\",\"TiPei\",\"Mumbai\"]},"
            + " {\"targetValue\": \"AfricaCity\", \"sourceValueIn\": [\"Addis Ababa\",\"Cairo\",\"Cape Town\",\"Lagos\"]}]";
    GeneralizeMaskingProviderConfig configuration = new GeneralizeMaskingProviderConfig();
    configuration.setMaskRuleSet(maskingOptionValue);

    GeneralizeMaskingProvider maskingProvider = new GeneralizeMaskingProvider(configuration);

    String identifier = "Dallas";
    String maskedValue = maskingProvider.mask(identifier);

    // The identifier "Dallas" should not be generalized, since it is not
    // listed
    assertTrue(maskedValue.equals("Dallas"));
  }

  @Test
  public void testPractitionerCityChicago() {
    String maskingOptionValue =
        "[{\"targetValue\": \"USCity\", \"sourceValueIn\": [\"New York\",\"Chicago\",\"Houston\",\"Minneapolis\",\"Boston\" ]},"
            + "{\"targetValue\": \"CanadaCity\", \"sourceValueIn\": [\"Toronto\",\"Montreal\",\"Vancouver\",\"Calgary\"]},"
            + "{\"targetValue\": \"Other\", \"sourceValueIn\": [\"*\"]}]";

    GeneralizeMaskingProviderConfig configuration = new GeneralizeMaskingProviderConfig();
    configuration.setMaskRuleSet(maskingOptionValue);

    GeneralizeMaskingProvider maskingProvider = new GeneralizeMaskingProvider(configuration);

    String identifier = "Chicago";
    String maskedValue = maskingProvider.mask(identifier);

    // The identifier "Chicago" should be generalized to "UsCity" since it
    // exists in USCity list of values
    assertTrue(maskedValue.equals("USCity"));
  }

  @Test
  public void testPractitionerCityMontreal() {
    String maskingOptionValue =
        "[{\"targetValue\": \"USCity\", \"sourceValueIn\": [\"New York\",\"Chicago\",\"Houston\",\"Minneapolis\",\"Boston\" ]},"
            + "{\"targetValue\": \"CanadaCity\", \"sourceValueIn\": [\"Toronto\",\"Montreal\",\"Vancouver\",\"Calgary\"]},"
            + "{\"targetValue\": \"Other\", \"sourceValueIn\": [\"*\"]}]";

    GeneralizeMaskingProviderConfig configuration = new GeneralizeMaskingProviderConfig();
    configuration.setMaskRuleSet(maskingOptionValue);

    GeneralizeMaskingProvider maskingProvider = new GeneralizeMaskingProvider(configuration);

    String identifier = "Montreal";
    String maskedValue = maskingProvider.mask(identifier);

    // The identifier "Montreal" should be generalized to "CanadaCity" since
    // it exists in CanadaCity list of values
    assertTrue(maskedValue.equals("CanadaCity"));
  }

  @Test
  public void testPractitionerCityOther() {
    String maskingOptionValue =
        "[{\"targetValue\": \"USCity\", \"sourceValueIn\": [\"New York\",\"Chicago\",\"Houston\",\"Minneapolis\",\"Boston\" ]},"
            + "{\"targetValue\": \"CanadaCity\", \"sourceValueIn\": [\"Toronto\",\"Montreal\",\"Vancouver\",\"Calgary\"]},"
            + "{\"targetValue\": \"Other\", \"sourceValueIn\": [\"*\"]}]";

    GeneralizeMaskingProviderConfig configuration = new GeneralizeMaskingProviderConfig();
    configuration.setMaskRuleSet(maskingOptionValue);

    GeneralizeMaskingProvider maskingProvider = new GeneralizeMaskingProvider(configuration);
    String identifier = "Dallas";
    String maskedValue = maskingProvider.mask(identifier);

    // The identifier "Dallas" should be generalized to "Other" since it is
    // not listed in the US or Canada cities and defaults to * rule
    assertTrue(maskedValue.equals("Other"));
  }

  @Test
  public void testNegationRuleDeviceComponentLanguageReplace() {
    String maskingOptionValue =
        "[{\"targetValue\": \"Other\", \"sourceValueNotIn\": [\"French\",\"Spanish\"]}]";

    GeneralizeMaskingProviderConfig configuration = new GeneralizeMaskingProviderConfig();
    configuration.setMaskRuleSet(maskingOptionValue);

    GeneralizeMaskingProvider maskingProvider = new GeneralizeMaskingProvider(configuration);

    String identifier = "Porteguses";
    String maskedValue = maskingProvider.mask(identifier);

    // The identifier "Porteguses" should be generalized to "Other" since it
    // is not one of the languages listed.
    assertTrue(maskedValue.equals("Other"));
  }

  @Test
  public void testNegationRuleDeviceComponentLanguageNotReplace() {
    String maskingOptionValue =
        "[{\"targetValue\": \"Other\", \"sourceValueNotIn\": [\"French\",\"Spanish\"]}]";

    GeneralizeMaskingProviderConfig configuration = new GeneralizeMaskingProviderConfig();
    configuration.setMaskRuleSet(maskingOptionValue);

    GeneralizeMaskingProvider maskingProvider = new GeneralizeMaskingProvider(configuration);

    String identifier = "Spanish";
    String maskedValue = maskingProvider.mask(identifier);

    // The identifier "Spanish" should not be generalized since is in the
    // list
    assertTrue(maskedValue.equals("Spanish"));
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
  public void testTagetValueParsingError() {
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
