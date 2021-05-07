/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;

import com.ibm.whc.deid.providers.identifiers.SSNUKIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.SSNUKMaskingProviderConfig;
import com.ibm.whc.deid.util.RandomGenerators;

public class SSNUKMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -5292579407685862344L;

  /*
   * The format of the number is two prefix letters, six digits, and one suffix letter.[5] The
   * example used is typically AB123456C. Often, the number is printed with spaces to pair off the
   * digits, like this: AB 12 34 56 C. Neither of the first two letters can be D, F, I, Q, U or V.
   * The second letter also cannot be O. The prefixes BG, GB, NK, KN, TN, NT and ZZ are not
   * allocated.[6] Validation lists of issued two-letter prefixes are published from time to
   * time.[7][8] The suffix letter is either A, B, C, or D.[5] (although F, M, and P have been used
   * for temporary numbers in the past)
   */
  private static final SSNUKIdentifier ssnukIdentifier = new SSNUKIdentifier();
  private static final char[] allowedFirstLetters = "ABCEGHJKLMNOPRSTWXYZ".toCharArray();
  private static final char[] allowedSecondLetters = "ABCEGHJKLMNPRSTWXYZ".toCharArray();
  private static final char[] allowedSuffixLetters = "ABCD".toCharArray();

  private final boolean preservePrefix;

  /** Instantiates a new Ssnuk masking provider. */
  public SSNUKMaskingProvider() {
    this(new SSNUKMaskingProviderConfig());
  }

  public SSNUKMaskingProvider(SSNUKMaskingProviderConfig configuration) {
    super(configuration);
    this.random = new SecureRandom();
    this.preservePrefix = configuration.isMaskPreservePrefix();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    String prefix = null;

    if (preservePrefix) {
      if (SSNUKMaskingProvider.ssnukIdentifier.isOfThisType(identifier)) {
        prefix = identifier.substring(0, 2);
      } else {
        if (isUnexpectedValueHandlingRandom()) {
          // add the debug message, but keep going to get a random value
          debugFaultyInput(identifier);
        } else {
          return applyUnexpectedValueHandling(identifier, null);
        }
      }
    }

    if (prefix == null) {
      prefix = "" + allowedFirstLetters[random.nextInt(allowedFirstLetters.length)];
      prefix += allowedSecondLetters[random.nextInt(allowedSecondLetters.length)];
    }

    StringBuilder builder = new StringBuilder(prefix);

    for (int i = 0; i < 6; i++) {
      char randomDigit = RandomGenerators.randomDigit();
      builder.append(randomDigit);
    }

    char suffix = allowedSuffixLetters[random.nextInt(allowedSuffixLetters.length)];
    builder.append(suffix);

    return builder.toString();
  }
}
