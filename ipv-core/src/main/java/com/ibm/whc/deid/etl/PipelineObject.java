/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.etl;

import com.ibm.whc.deid.util.Tuple;

public interface PipelineObject {
  /**
   * Apply tuple.
   *
   * @param input the input
   * @return the tuple
   */
  Tuple<String, Boolean> apply(String input);
}
