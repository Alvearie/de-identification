/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import java.security.SecureRandom;

/**
 * A token class and multiplicity to build a part of the string with
 *
 */
public class BuildStep implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -5103556882153858880L;
// The token class to generate with
  private TokenSet c;
  // Number of characters to gen is determined from this
  private Multiplicity m;

  /**
   * Create a build step from a token class and a multiplicity
   *
   * @param lastClassLoaded The token class
   * @param mult The multiplicity. This can be null, which implies a multiplicity of 1
   */
  public BuildStep(TokenSet lastClassLoaded, Multiplicity mult) {
    this.c = lastClassLoaded;
    this.m = mult;
  }

  /**
   * Set the multiplicity individually of the token class
   *
   * @param mult The multiplicity
   */
  public void setMultiplicity(Multiplicity mult) {
    this.m = mult;
  }

  /**
   * Generate a random string based on this build step. This will be a part of the entire pattern
   *
   * @param random A SecureRandom instance to generate with.
   * @return A string generated based on the given token class and multiplicity
   */
  public String generateRandomString(SecureRandom random) {
    int times = 1;
    String returnString = "";
    // Get a random multiplicity
    if (m != null) {
      times = m.getRandomMultiplicity(random);
    }
    // Get the random token the appropriate number of times.
    for (int i = 0; i < times; i++) {
      returnString += c.getRandomToken(random);
    }
    return returnString;
  }
}
