/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.seceng;

/** Defines bounding limits set for Secure Engineering for utilities. */
public interface SecEngBoundingLimits {

  final int MAX_READ_LIMIT = Integer.MAX_VALUE - 8096;
  final int DEFAULT_READ_LIMIT = 1024 * 1024; // 1M chars
  final int DEFAULT_READ_CHAR_LIMIT = DEFAULT_READ_LIMIT;
}
