/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.etl.objects;

import com.ibm.whc.deid.etl.PipelineObject;
import com.ibm.whc.deid.util.Tuple;
import java.util.regex.Pattern;

public class Filter implements PipelineObject {
  private final Pattern pattern;

  /**
   * Instantiates a new Filter.
   *
   * @param regexPattern the regex pattern
   */
  public Filter(String regexPattern) {
    this.pattern = Pattern.compile(regexPattern);
  }

  @Override
  public Tuple<String, Boolean> apply(String input) {
    boolean match = pattern.matcher(input).matches();
    return new Tuple<>(null, match ? Boolean.TRUE : Boolean.FALSE);
  }
}
