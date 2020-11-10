/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

public interface PointTranslator {
  /**
   * Translate.
   *
   * @param point the point
   */
  void translate(double[] point);
}
