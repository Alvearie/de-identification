/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.util.Iterator;
import java.util.List;

/**
 * A parser for reverse regex. This class provides a basic 2 lookahead parser with unicode support
 * and some utility methods for escapes.
 *
 */
public class ReverseRegexParser {
  /** The set of characters to parse */
  protected List<String> characters;

  /** An iterator of these characters */
  protected Iterator<String> charIt;

  // The current character in the parse logic
  protected String character = null;

  // The next character in the parse logic
  protected String nextCharacter = null;

  /** For empty parser */
  public ReverseRegexParser() {
    characters = null;
    charIt = null;
  }

  /** For a populated parser */
  public ReverseRegexParser(List<String> characters2) {
    characters = characters2;
    charIt = characters2.iterator();
  }

  /** Set character and nextCharacter ahead once */
  public void getNewChar(boolean combineSurrogates) {
    character = nextCharacter;
    if (charIt.hasNext()) {
      nextCharacter = charIt.next();
    } else {
      nextCharacter = null;
    }
    if (combineSurrogates) {
      checkUnicodeSurrogates();
    }
  }

  /** Set character and nextCharacter ahead twice */
  public void getNewCharAndNewNextChar(boolean combineSurrogates) {
    if (charIt.hasNext()) {
      character = charIt.next();
    } else {
      character = null;
    }
    if (charIt.hasNext()) {
      nextCharacter = charIt.next();
    } else {
      nextCharacter = null;
    }
    if (combineSurrogates) {
      checkUnicodeSurrogates();
    }
  }

  /** Combine any UTF16 surrogates into a single string */
  public void checkUnicodeSurrogates() {
    if (character == null || nextCharacter == null)
      return;
    if (Character.isSurrogate(character.charAt(0))) {
      character = character + nextCharacter;
      if (charIt.hasNext()) {
        nextCharacter = charIt.next();
      } else {
        nextCharacter = null;
      }
    }
  }

  /**
   * Handle escape sequences. This method expects the "character" of the Parser superclass be set to
   * the character escaped.
   *
   * @return A Token set representative of the class. Null upon failure
   */
  protected TokenSet handleEscapes() {
    TokenSet ts = null;
    switch (character) {
      case "d":
        ts = CharacterRange.ENGLISH_DIGITS;
        break;
      case "u":
        ts = CharacterRange.ENGLISH_LETTERS_CAPS;
        break;
      case "l":
        ts = CharacterRange.ENGLISH_LETTERS_LOWER;
        break;
      case "p":
        Integer from = handleHexCode();
        Integer to = null;
        if (nextCharacter != null && nextCharacter.equals("-")) {
          getNewCharAndNewNextChar(false);
          if (character != null && character.equals("\\") && nextCharacter != null
              && nextCharacter.equals("p")) {
            getNewChar(false);
            to = handleHexCode();
          }
        }
        if (from != null) {
          if (to != null) {
            ts = new CharacterRange(from, to);
          } else {
            ts = new CharacterRange(from);
          }
        }
        break;
      default:
        ts = new CharacterRange(character);
        break;
    }
    return ts;
  }

  /**
   * Parse an 8 byte hex unicode character
   *
   * @return An integer representing the code point. Null if fails.
   */
  private Integer handleHexCode() {
    String hexCode = "";
    for (int i = 0; i < 8; i++) {
      if (nextCharacter != null) {
        hexCode += nextCharacter;
        getNewChar(false);
      }
    }
    // Parse hex code for a UTF16 number and then return that character.
    if (hexCode.length() == 8) {
      try {
        return Integer.parseUnsignedInt(hexCode, 16);
      } catch (NumberFormatException e) {
        return null;
      }
    }
    return null;
  }
}
