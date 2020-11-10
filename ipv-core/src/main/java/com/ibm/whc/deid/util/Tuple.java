/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

/**
 * @param <K> the type parameter
 * @param <V> the type parameter
 */
public class Tuple<K, V> {
  /** The First. */
  final K first;
  /** The Second. */
  final V second;

  /**
   * Instantiates a new Tuple.
   *
   * @param first the first
   * @param second the second
   */
  public Tuple(K first, V second) {
    this.first = first;
    this.second = second;
  }

  /**
   * Gets first.
   *
   * @return the first
   */
  public K getFirst() {
    return first;
  }

  /**
   * Gets second.
   *
   * @return the second
   */
  public V getSecond() {
    return second;
  }
}
