/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.pojo.config.masking.RandomMaskingProviderConfig;
import com.ibm.whc.deid.util.RandomGenerators;

public class RandomMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -2866608598077287753L;

  public RandomMaskingProvider() {
    super(new RandomMaskingProviderConfig());
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    return RandomGenerators.randomReplacement(identifier);
  }
}
