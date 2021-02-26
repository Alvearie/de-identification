/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.pojo.config.masking.ReplaceMaskingProviderConfig;
import com.ibm.whc.deid.util.RandomGenerators;

/**
 * The type Truncate masking provider.
 *
 */
public class ReplaceMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 8734109695774758607L;

  private final int preservedCharacters;
  private final int offset;
  private final boolean replaceWithAsterisks;
  private final boolean replaceWithRandom;


  /**
   * Instantiates a new Replace masking provider.
   *
   * @param configuration the configuration
   */
  public ReplaceMaskingProvider(ReplaceMaskingProviderConfig configuration) {
    this.offset = configuration.getMaskOffset();
    this.preservedCharacters = configuration.getMaskPreserve();
    this.replaceWithRandom = configuration.isMaskReplaceWithRandom();
    this.replaceWithAsterisks =
        this.replaceWithRandom ? false : configuration.isMaskReplaceWithAsterisks();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    int identifierLength = identifier.length();
    if (offset >= identifierLength) {
      return null;
    }

    int stop = offset + preservedCharacters;
    if (stop > identifierLength) {
      stop = identifierLength;
    }

    String maskedValue = identifier.substring(this.offset, stop);
    if (!this.replaceWithAsterisks && !this.replaceWithRandom) {
      return maskedValue;
    }

    StringBuilder builder = new StringBuilder();

    if (this.offset > 0) {
      if (this.replaceWithAsterisks) {
        for (int i = 0; i < this.offset; i++) {
          char current = identifier.charAt(i);
          if (isCharacter(current)) {
            builder.append(current);
            continue;
          }
          builder.append('*');
        }
      } else {
        builder.append(RandomGenerators.randomReplacement(identifier.substring(0, this.offset)));
      }
    }

    builder.append(maskedValue);

    if (stop < identifierLength) {
      if (this.replaceWithAsterisks) {
        for (int i = stop; i < identifierLength; i++) {
          char current = identifier.charAt(i);
          if (isCharacter(current)) {
            builder.append(current);
            continue;
          }
          builder.append('*');
        }
      } else {
        builder.append(
            RandomGenerators.randomReplacement(identifier.substring(stop, identifierLength)));
      }
    }

    return builder.toString();
  }

  private static boolean isCharacter(char c) {
    if (Character.isAlphabetic(c) || Character.isDigit(c)) {
      return false;
    }
    return true;
  }
}
