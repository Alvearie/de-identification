/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.security.SecureRandom;

public interface TokenSet {

  /**
   * A random token from this set, consisting of a string of one or more characters
   *
   * @param s A SecureRandom instance to generate with
   * @return A String representing a random selection from this token set. If the generation failed,
   *         a null will be returned.
   */
  String getRandomToken(SecureRandom s);

  /**
   * A specific token, by index.
   *
   * @param index The integer index of the token. This index will begin at 1 and is the size of the
   *        token set. For example, the TokenClass [abc] has an index from 0 to 2 for the 3 options,
   *        and the class representing (a|[abc]) would have 0 to 1, 0 being the character a and 1
   *        calling the TokenClass instance to pick randomly from a-c.
   * @return The token, as a string. If the index is out of bounds, a null is returned
   */
  String getTokenAt(int index);

  /**
   * The total size of the token set.
   *
   * @return Total size as an integer. Empty sets will get 0.
   */
  int getSize();
}
