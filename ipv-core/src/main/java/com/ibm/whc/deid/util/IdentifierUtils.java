/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import com.ibm.whc.deid.providers.identifiers.CharacterRequirements;

public class IdentifierUtils {
  /**
   * Check last digit boolean.
   *
   * @param ccNumber the cc number
   * @return the boolean
   */
  public static boolean checkLastDigit(String ccNumber) {
    int sum = 0;
    boolean alternate = false;
    for (int i = ccNumber.length() - 1; i >= 0; i--) {
      int n = ccNumber.charAt(i) - '0';
      if (alternate) {
        n *= 2;
        if (n > 9) {
          n = (n % 10) + 1;
        }
      }
      sum += n;
      alternate = !alternate;
    }
    return (sum % 10 == 0);
  }

  public static boolean looksLikeFreeText(String value) {
    if (value.length() < 10) {
      return false;
    }

    int whitespaceRegions = 0;

    for (int i = 0; i < value.length() && whitespaceRegions < 2; i++) {
      char c = value.charAt(i);
      if (Character.isWhitespace(c)) {
        whitespaceRegions++;
        while (i < value.length() && Character.isWhitespace(value.charAt(i))) {
          i++;
        }
      }
    }

    return whitespaceRegions >= 2;
  }

  public static int createCharacterProfile(String input) {
    int mask = CharacterRequirements.NONE;

    for (int i = 0; i < input.length(); i++) {
      char c = input.charAt(i);

      if (Character.isDigit(c)) {
        mask |= CharacterRequirements.DIGIT;
      } else if (Character.isAlphabetic(c)) {
        mask |= CharacterRequirements.ALPHA;
      } else if (Character.isWhitespace(c)) {
        mask |= CharacterRequirements.SPACE;
      } else if (c == '.') {
        mask |= CharacterRequirements.DOT;
      } else if (c == '@') {
        mask |= CharacterRequirements.AT;
      } else if (c == '-') {
        mask |= CharacterRequirements.DASH;
      } else if (c == ':') {
        mask |= CharacterRequirements.DOUBLEDOT;
      }
    }

    return mask;
  }
}
