/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import java.security.SecureRandom;

/**
 * Unicode compliant CharacterRange. This represents a range of characters and should be compliant
 * with UTF-16, with built in support for Unicode if 2 byte points are used in the string.
 *
 * <p>
 * Implements the TokenSet interface to be used when building strings.
 *
 */
public class CharacterRange implements TokenSet, Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7775741633828291049L;

// Start of the range
  private Integer start = null;

  // End of the range
  private Integer end = null;

  // Length of the range.
  private int size;

  /**
   * Create a Character Range using the 2 integers. The range is omnidirectional; order does not
   * matter.
   *
   * @param start An integer representing a unicode character
   * @param end An integer representing a unicode character
   */
  public CharacterRange(int start, int end) {
    putToParameters(start, end);
  }

  /**
   * Create a Character Range using the 2 chars. Since java chars are UTF16, the charset is limited
   * to those code points. The range is omnidirectional; order does not matter.
   *
   * @param start A char representing a UTF16 character
   * @param end An char representing a UTF character
   */
  public CharacterRange(char start, char end) {
    putToParameters(start, end);
  }

  /**
   * A single character range
   *
   * @param character A single character in UTF16
   */
  public CharacterRange(char character) {
    putToParameters(character, character);
  }

  /**
   * A single character range
   *
   * @param character A single integer code point in Unicode
   */
  public CharacterRange(int character) {
    putToParameters(character, character);
  }

  /**
   * A single Unicode string-encoded character
   *
   * @param character A string containing a unicode character (2 byte characters supported)
   */
  public CharacterRange(String character) {
    putToParameters(character.codePointAt(0), character.codePointAt(0));
  }

  /**
   * A range of Unicode characters represented between the first unicode point of the parameters.
   * Two byte characters are supported. The range is omnidirectional; order does not matter.
   *
   * @param start A string containing a unicode character (2 byte characters supported)
   * @param end A string containing a unicode character (2 byte characters supported)
   */
  public CharacterRange(String start, String end) {
    putToParameters(start.codePointAt(0), end.codePointAt(0));
  }

  @Override
  public String getRandomToken(SecureRandom random) {
    if (size == 1) {
      return String.valueOf(Character.toChars(start));
    }
    int randomInt = random.nextInt(end - start);
    return String.valueOf(Character.toChars(start + randomInt));
  }

  /**
   * Put the start and end characters in to the instance vars, reversing the order if need be.
   *
   * @param start the starting Unicode character
   * @param end the ending Unicode character
   */
  private void putToParameters(int start, int end) {
    if (start > end) {
      int temp = start;
      start = end;
      end = temp;
    }
    this.end = end;
    this.start = start;
    this.size = end - start + 1;
  }

  @Override
  public String getTokenAt(int index) {
    return String.valueOf(Character.toChars(start + index));
  }

  @Override
  public int getSize() {
    return size;
  }

  public static final CharacterRange ENGLISH_DIGITS = new CharacterRange('0', '9');

  public static final CharacterRange ENGLISH_LETTERS_CAPS = new CharacterRange('A', 'Z');

  public static final CharacterRange ENGLISH_LETTERS_LOWER = new CharacterRange('a', 'z');
}
