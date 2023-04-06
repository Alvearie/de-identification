/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.shared.pojo.config.masking.PseudonymMaskingProviderConfig;
import org.junit.Test;

public class PseudonymMaskingProviderTest extends TestLogSetUp {

  @Test
  public void testGenerateViaOptions_Default() throws Exception {
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider();

    final String originalValue = "origi^@l";
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue.length() == 10);
    assertTrue(!maskedValue.equals(originalValue));
  }

  @Test
  public void testGenerateViaOptions_NotEnabled() throws Exception {
    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaOptionsGenerateDigit(true);
    configuration.setGenerateViaOptionsGenerateSpecial(true);
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);

    final String originalValue = "original";
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue.equals(originalValue));
  }

  @Test
  public void testGenerateViaOptions_MinMaxLength() throws Exception {
    final String originalValue = "original";

    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaOptionsEnabled(true);
    configuration.setGenerateViaOptionsGenerateLowercase(true);
    configuration.setGenerateViaOptionsGenerateUppercase(true);
    configuration.setGenerateViaOptionsMinLength(5);
    configuration.setGenerateViaOptionsMaxLength(5);
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);

    String maskedValue = maskingProvider.mask(originalValue);
    assertTrue(maskedValue.length() == 5);

    configuration.setGenerateViaOptionsMinLength(6);
    configuration.setGenerateViaOptionsMaxLength(7);
    maskingProvider = new PseudonymMaskingProvider(configuration);

    maskedValue = maskingProvider.mask(originalValue);
    assertTrue(maskedValue.length() == 6 || maskedValue.length() == 7);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGenerateViaOptions_MinMaxLength_Invalid() throws Exception {
    final String originalValue = "original";
    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaOptionsEnabled(true);
    configuration.setGenerateViaOptionsGenerateLowercase(true);
    configuration.setGenerateViaOptionsGenerateUppercase(true);
    configuration.setGenerateViaOptionsGenerateDigit(true);
    configuration.setGenerateViaOptionsMinLength(7);
    configuration.setGenerateViaOptionsMaxLength(6);
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);

    String maskedValue = maskingProvider.mask(originalValue);
    assertEquals(null, maskedValue);
  }

  @Test
  public void testGenerateViaOptions_Uppercase() throws Exception {
    final String originalValue = "original";
    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaOptionsEnabled(true);
    configuration.setGenerateViaOptionsMinLength(20);
    configuration.setGenerateViaOptionsMaxLength(20);
    configuration.setGenerateViaOptionsGenerateUppercase(true);
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue.length() == 20);
    assertTrue(isValidMaskedValue(maskedValue, true, false, false, false, false));
  }

  @Test
  public void testGenerateViaOptions_Lowercase() throws Exception {
    final String originalValue = "ORIGINAL";
    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaOptionsEnabled(true);
    configuration.setGenerateViaOptionsMinLength(20);
    configuration.setGenerateViaOptionsMaxLength(20);
    configuration.setGenerateViaOptionsGenerateLowercase(true);
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue.length() == 20);
    assertTrue(isValidMaskedValue(maskedValue, false, true, false, false, false));
  }

  @Test
  public void testGenerateViaOptions_Digit() throws Exception {
    final String originalValue = "original";
    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaOptionsEnabled(true);
    configuration.setGenerateViaOptionsMinLength(20);
    configuration.setGenerateViaOptionsMaxLength(20);
    configuration.setGenerateViaOptionsGenerateDigit(true);
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue.length() == 20);
    assertTrue(isValidMaskedValue(maskedValue, false, false, true, false, false));
  }

  @Test
  public void testGenerateViaOptions_Special() throws Exception {
    final String originalValue = "original";

    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaOptionsEnabled(true);
    configuration.setGenerateViaOptionsMinLength(20);
    configuration.setGenerateViaOptionsMaxLength(20);
    configuration.setGenerateViaOptionsGenerateSpecial(true);
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue.length() == 20);
    assertTrue(isValidMaskedValue(maskedValue, false, false, false, true, false));
  }

  @Test
  public void testGenerateViaOptions_Combinations() throws Exception {
    final String originalValue = "original";

    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaOptionsEnabled(true);
    configuration.setGenerateViaOptionsMinLength(250);
    configuration.setGenerateViaOptionsMaxLength(250);
    configuration.setGenerateViaOptionsGenerateUppercase(true);
    configuration.setGenerateViaOptionsGenerateLowercase(true);
    configuration.setGenerateViaOptionsGenerateDigit(true);
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue.length() == 250);
    assertTrue(isValidMaskedValue(maskedValue, true, true, true, false, false));

    configuration.setGenerateViaOptionsGenerateSpecial(true);

    maskingProvider = new PseudonymMaskingProvider(configuration);
    maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue.length() == 250);
    assertTrue(isValidMaskedValue(maskedValue, true, true, true, true, false));
  }

  @Test
  public void testGenerateViaPattern_NotEnabled() throws Exception {
    final String originalValue = "original";
    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaPatternPattern("[\\d]{5}");
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);
    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue.equals(originalValue));
  }

  @Test
  public void testGenerateViaPattern_PatternWithNoMultiplicity() throws Exception {
    final String originalValue = "original";
    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaPatternEnabled(true);
    configuration.setGenerateViaPatternPattern("[\\d]");
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);

    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue.length() == 1);
    assertTrue(isValidMaskedValue(maskedValue, false, false, true, false, false));
  }

  @Test
  public void testGenerateViaPattern_PatternWithMultiplicity() throws Exception {
    final String originalValue = "original";
    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaPatternEnabled(true);
    configuration.setGenerateViaPatternPattern("[\\u]{5}");
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);

    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue.length() == 5);
    assertTrue(isValidMaskedValue(maskedValue, true, false, false, false, false));
  }

  @Test
  public void testGenerateViaPattern_PatternWithMultiplicityRange() throws Exception {
    final String originalValue = "original";
    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaPatternEnabled(true);
    configuration.setGenerateViaPatternPattern("[\\l]{8,9}");
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);

    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue.length() == 8 || maskedValue.length() == 9);
    assertTrue(isValidMaskedValue(maskedValue, false, true, false, false, false));
  }

  @Test
  public void testGenerateViaPattern_PatternWithOrGroups() throws Exception {
    final String originalValue = "original";
    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaPatternEnabled(true);
    configuration.setGenerateViaPatternPattern("([%]{2}|[&])");
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);

    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(
        (maskedValue.length() == 2 && maskedValue.charAt(0) == '%' && maskedValue.charAt(1) == '%')
            || (maskedValue.length() == 1 && maskedValue.charAt(0) == '&'));
  }

  @Test
  public void testGenerateViaPattern_NonExistingPatternName() throws Exception {
    final String originalValue = "original";

    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaPatternEnabled(true);
    configuration.setGenerateViaPatternPatternName("DOES_NOT_EXIST");
    configuration.setGenerateViaPatternLanguageCode("EN");
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);

    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue("".equals(maskedValue));

    // whitespace (missing) pattern instead of null
    configuration.setGenerateViaPatternPattern("  ");
    maskingProvider = new PseudonymMaskingProvider(configuration);
    maskedValue = maskingProvider.mask(originalValue);
    assertTrue("".equals(maskedValue));
  }

  @Test
  public void testHash_NotEnabled() throws Exception {
    final String originalValue = "original";
    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaHashUseSHA256(true);
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);

    String maskedValue = maskingProvider.mask(originalValue);

    assertTrue(maskedValue.equals(originalValue));
  }

  @Test
  public void testHash_SHA256() throws Exception {
    final String originalValue = "original";
    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaHashEnabled(true);
    configuration.setGenerateViaHashUseSHA256(true);
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);

    String maskedValue = maskingProvider.mask(originalValue).toLowerCase();

    assertTrue(
        "0682c5f2076f099c34cfdd15a9e063849ed437a49677e6fcc5b4198c76575be5".equals(maskedValue));
  }

  @Test
  public void testHash_SHA512() throws Exception {
    final String originalValue = "original";
    PseudonymMaskingProviderConfig configuration = new PseudonymMaskingProviderConfig();
    setAllPseudonymMaskingToFalse(configuration);
    configuration.setGenerateViaHashEnabled(true);
    configuration.setGenerateViaHashUseSHA256(false);
    PseudonymMaskingProvider maskingProvider = new PseudonymMaskingProvider(configuration);

    String maskedValue = maskingProvider.mask(originalValue).toLowerCase();

    assertTrue(
        "c5ee067fb433795d5c8efeca78623791dc6ce524198b7223fe8310f81a38c9105da8a61714dd5a633e52dac7b57b33948afd94cb37c522f89781c9c25471a9c3"
            .equals(maskedValue));
  }

  /**
   * Determines if the masked value is valid given the specified options.
   *
   * @param uppercase true if masked value can contain uppercase letters, otherwise false
   * @param lowercase true if masked value can contain lowercase letters, otherwise false
   * @param digit true if masked value can contain digits, otherwise false
   * @param special true if masked value can contain special characters, otherwise false
   * @param other true if masked value can contain any other characters, otherwise false
   */
  private boolean isValidMaskedValue(String maskedValue, boolean uppercase, boolean lowercase,
      boolean digit, boolean special, boolean other) {
    char ch;
    boolean hasSpecial = false;
    boolean hasDigit = false;
    boolean hasUppercase = false;
    boolean hasLowerCase = false;
    boolean hasOther = false;
    for (int i = 0; i < maskedValue.length(); ++i) {
      ch = maskedValue.charAt(i);
      if (ch == '!' || ch == '@' || ch == '#' || ch == '$' || ch == '%' || ch == '^' || ch == '&'
          || ch == '*' || ch == '[' || ch == ']' || ch == '\\' || ch == '/' || ch == '?'
          || ch == '{' || ch == '}' || ch == '+' || ch == '-' || ch == '_') {
        hasSpecial = true;
      } else if (Character.isDigit(ch)) {
        hasDigit = true;
      } else if (Character.isUpperCase(ch)) {
        hasUppercase = true;
      } else if (Character.isLowerCase(ch)) {
        hasLowerCase = true;
      } else {
        hasOther = true;
      }
    }

    return (uppercase == hasUppercase) && (lowercase == hasLowerCase) && (digit == hasDigit)
        && (special == hasSpecial) && (other == hasOther);
  }

  private void setAllPseudonymMaskingToFalse(PseudonymMaskingProviderConfig configuration) {
    configuration.setGenerateViaOptionsEnabled(false);
    configuration.setGenerateViaOptionsGenerateLowercase(false);
    configuration.setGenerateViaOptionsGenerateUppercase(false);
    configuration.setGenerateViaOptionsGenerateDigit(false);
    configuration.setGenerateViaOptionsGenerateSpecial(false);
    configuration.setGenerateViaPatternEnabled(false);
    configuration.setGenerateViaHashEnabled(false);
  }
}
