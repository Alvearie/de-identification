/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import java.security.SecureRandom;
import java.util.ArrayList;

/**
 * Represents a range of multiplicities, for use in a build step
 *
 */
public class Multiplicity extends ReverseRegexParser implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 6860251015846620201L;
	private int lower = 1;
  private int upper;
  private boolean singleValue = true;

  /**
   * A random multiplicity given the range
   *
   * @param random An instance of SecureRandom to generate with
   * @return A random integer between the minimum and maximum inclusive
   */
  public int getRandomMultiplicity(SecureRandom random) {
    if (singleValue)
      return lower;
    return random.nextInt((upper - lower) + 1) + lower;
  }

  /**
   * Parse a multiplicity from a string
   *
   * @param pattern Not currently used
   * @param multiPattern A multiplicity string, of the form \d+(,\d+)? For exmaple, 2,3 would be 2
   *        or 3 2 would be 2 every time
   */
  public Multiplicity(ReversePatternGenerator pattern, ArrayList<String> multiPattern) {
    super(multiPattern);
    String digitsMin = null;
    String digitsMax = null;
    boolean seenComma = false;
    getNewCharAndNewNextChar(false);
    // Get the first digits
    while (character != null) {
      if (character.matches("\\s")) {
        getNewChar(false);
        continue;
      }
      if (character.matches("\\d")) {
        if (!seenComma) {
          if (digitsMin == null) {
            digitsMin = "";
          }
          digitsMin += character;
        } else {
          if (digitsMax == null) {
            digitsMax = "";
          }
          digitsMax += character;
        }
        getNewChar(false);
        continue;
      }
      // if there is a comma, discard it and move on to the next digit
      if (character.equals(",")) {
        seenComma = true;
        getNewChar(false);
        continue;
      }
      break;
    }
    // Parse the integers. If the parse fails, it will use the first
    // digit if parsed and 1 if not.
    try {
      lower = Integer.parseInt(digitsMin);
      if (digitsMax != null) {
        upper = Integer.parseInt(digitsMax);
      } else {
        upper = lower;
      }
    } catch (NumberFormatException e) {
      upper = 1;
      lower = 1;
    }
    singleValue = (upper == lower);
  }
}
