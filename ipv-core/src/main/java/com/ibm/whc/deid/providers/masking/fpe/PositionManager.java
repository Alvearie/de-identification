/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

/**
 * Utility class that tracks and categorizes the content at each position of an input string.
 */
public class PositionManager {

  public enum CharType {
    // 0-9
    DIGIT,

    // A-Z
    UPPER,

    // a-z
    LOWER,

    // anything else
    OTHER
  }

  public class Position {
    private char original;
    private CharType type;

    public Position(char ch) {
      original = ch;
      if (ch >= '0' && ch <= '9') {
        type = CharType.DIGIT;
      } else if (ch >= 'a' && ch <= 'z') {
        type = CharType.LOWER;
      } else if (ch >= 'A' && ch <= 'Z') {
        type = CharType.UPPER;
      } else {
        type = CharType.OTHER;
      }
    }

    public char getOriginal() {
      return original;
    }

    public CharType getType() {
      return type;
    }
  }

  private Position[] positions;

  /**
   * Initializes a new PositionManager with the contents of the given string.
   * 
   * @param original the non-null string to examine
   */
  public PositionManager(String original) {
    int len = original.length();
    positions = new Position[len];
    for (int i = 0; i < len; i++) {
      positions[i] = new Position(original.charAt(i));
    }
  }

  public Position[] getPositions() {
    return positions;
  }

  /**
   * Assemble all the characters of the indicated categories into a single string in the order in
   * which the characters appear in the original.
   * 
   * @param includeDigits include digits in the output
   * @param includeLower include lower case characters (a-z) in the output
   * @param includeUpper include upper case characters (A-Z) in the output
   * 
   * @return the new, possibly-empty string
   */
  public String extract(boolean includeDigits, boolean includeLower, boolean includeUpper) {
    int length = positions.length;
    StringBuilder buffer = new StringBuilder(length);
    for (Position p : positions) {
      switch (p.getType()) {
        case DIGIT:
          if (includeDigits) {
            buffer.append(p.getOriginal());
          }
          break;
        case LOWER:
          if (includeLower) {
            buffer.append(p.getOriginal());
          }
          break;
        case UPPER:
          if (includeUpper) {
            buffer.append(p.getOriginal());
          }
          break;
        default:
          // nothing required for other types
          break;
      }
    }
    return buffer.toString();
  }

  /**
   * Assemble a new string in the same format as the original string by merging characters from the
   * original and a given string. If the original character at a given position is from an indicated
   * category, its value comes from the given string. If not, its value is the original character
   * that was at that position. If the given string has more data than is necessary for this
   * process, the extraneous characters are ignored.
   * 
   * @param encrypted the new given string providing new data into the merged string
   * 
   * @param includeDigits replace digits (0-9) with the next character from the new given string
   * @param includeLower replace lower case characters (a-z) with the next character from the new
   *        given string
   * @param includeUpper replace upper case characters (A-Z) with the next character from the new
   *        given string
   * 
   * @return the newly assembled from merged data
   * 
   * @throws RuntimeException if the new given string has fewer characters than are needed to
   *         replace all the indicated characters in the original string
   */
  public String replaceSymbols(String encrypted, boolean includeDigits, boolean includeLower,
      boolean includeUpper) {
    int encryptedIndex = 0;
    StringBuilder buffer = new StringBuilder(encrypted.length());
    for (Position p : positions) {
      switch (p.getType()) {
        case DIGIT:
          buffer.append(includeDigits ? encrypted.charAt(encryptedIndex++) : p.getOriginal());
          break;
        case LOWER:
          buffer.append(includeLower ? encrypted.charAt(encryptedIndex++) : p.getOriginal());
          break;
        case UPPER:
          buffer.append(includeUpper ? encrypted.charAt(encryptedIndex++) : p.getOriginal());
          break;
        default:
          buffer.append(p.getOriginal());
          break;
      }
    }
    return buffer.toString();
  }
}
