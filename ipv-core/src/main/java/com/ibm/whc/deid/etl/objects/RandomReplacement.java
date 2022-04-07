/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.etl.objects;

import com.ibm.whc.deid.etl.PipelineObject;
import com.ibm.whc.deid.util.RandomGenerators;
import com.ibm.whc.deid.util.Tuple;

public class RandomReplacement implements PipelineObject {
  private final int offset;
  private final int depth;

  /**
   * Instantiates a new Random replacement.
   *
   * @param offset the offset
   * @param depth the depth
   */
  public RandomReplacement(int offset, int depth) {
    this.offset = offset;
    this.depth = depth;
  }

  @Override
  public Tuple<String, Boolean> apply(String input) {
    int inputLength = input.length();

    if (offset >= inputLength) {
      return new Tuple<>(input, Boolean.TRUE);
    }

    int end = offset + depth;
    if (end > inputLength) {
      end = inputLength;
    }

    String maskedValue = RandomGenerators.randomReplacement(input.substring(offset, end));
    return new Tuple<>(maskedValue, Boolean.TRUE);
  }
}
