/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import java.security.SecureRandom;
import java.util.List;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.TreeMap;

/**
 * Represents a set of characters or tokens / token sets to randomly select from.
 *
 */
public class TokenClass extends ReverseRegexParser implements TokenSet, Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 4435844232861484662L;
// Sets of tokens in this class. Can include any instance of a class
  // implementing
  // TokenSet
  private transient volatile NavigableMap<Integer, TokenSet> characterSets;
  private int totalSize = 0;

  /**
   * Create a token class based on a list of characters forming a pattern
   *
   * @param characters the characters
   */
  public TokenClass(List<String> characters) {
    super(characters);
    characterSets = new TreeMap<Integer, TokenSet>();

    getNewCharAndNewNextChar(true);
    while (character != null) {
      // Get any escapes
      if (character.equals("\\")) {
        getNewChar(true);
        TokenSet ts = handleEscapes();
        if (ts != null) {
          addCharacterSet(ts);
        }
        getNewChar(true);
        continue;
      }
      // If there is a token, is it a range?
      if (nextCharacter != null && nextCharacter.equals("-")) {
        String thisCharacter = character;
        getNewCharAndNewNextChar(true);
        if (character != null) {
          // Support full unicode space ranges
          checkUnicodeSurrogates();
          addCharacterSet(new CharacterRange(thisCharacter, character));
          getNewChar(true);
          continue;
        } else {
          addCharacterSet(new CharacterRange(thisCharacter));
        }
      }
      if (character != null) {
        addCharacterSet(new CharacterRange(character));
      }
      getNewChar(true);
    }
  }

  /** Empty token class */
  public TokenClass() {
    characterSets = new TreeMap<Integer, TokenSet>();
    totalSize = 0;
  }

  /**
   * Token class with a single set of tokens.
   *
   * @param c The token set.
   */
  public TokenClass(TokenSet c) {
    characterSets = new TreeMap<Integer, TokenSet>();
    totalSize = 0;
    characterSets.put(totalSize, c);
    totalSize += c.getSize();
  }

  /**
   * Add a token set to an existing class. The total size will be updated.
   *
   * @param charSet The set to add.
   */
  public void addCharacterSet(TokenSet charSet) {
    characterSets.put(totalSize, charSet);
    totalSize += charSet.getSize();
  }

  @Override
  public String getTokenAt(int key) {
    Entry<Integer, TokenSet> entry = characterSets.floorEntry(key);
    if (entry == null)
      return "";
    return String.valueOf(entry.getValue().getTokenAt(key - entry.getKey()));
  }

  @Override
  public String getRandomToken(SecureRandom s) {
    if (totalSize == 0)
      return "";
    int selection = s.nextInt(totalSize);
    return getTokenAt(selection);
  }

  @Override
  public int getSize() {
    return totalSize;
  }
}
