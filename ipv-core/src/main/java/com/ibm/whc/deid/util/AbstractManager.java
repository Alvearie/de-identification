/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.util.Collection;

/**
 * @param <K> the type parameter
 */
public abstract class AbstractManager<K> implements Manager {
  /**
   * Gets item list.
   *
   * @return the item list
   */
  public abstract Collection<K> getItemList();
}
