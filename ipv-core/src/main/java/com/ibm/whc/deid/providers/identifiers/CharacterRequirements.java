/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

public class CharacterRequirements {
  public static final int NONE = 0;
  public static final int DIGIT = 1;
  public static final int ALPHA = 2;
  public static final int AT = 4;
  public static final int DOT = 8;
  public static final int SPACE = 16;
  public static final int DASH = 32;
  public static final int DOUBLEDOT = 64;
}
