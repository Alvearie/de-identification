/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.Serializable;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @param <K> the key type parameter
 * @param <V> the value type parameter
 */
public class MapWithRandomPick<K, V> implements Serializable {

  private static final long serialVersionUID = -1788413492014517734L;

  private final Map<K, V> map;
  private final List<K> keylist = new ArrayList<>();
  private final SecureRandom random;

  /**
   * Instantiates a new Map with random pick.
   *
   * @param map the map
   */
  public MapWithRandomPick(Map<K, V> map) {
    this.map = map;
    this.random = new SecureRandom();
  }

  /** Sets key list. */
  public void setKeyList() {

    Set<K> keyset = map.keySet();
    Iterator<K> iterator = keyset.iterator();

    keylist.clear();
    while (iterator.hasNext()) {
      K item = iterator.next();
      keylist.add(item);
    }
  }

  /**
   * Gets map.
   *
   * @return the map
   */
  public Map<K, V> getMap() {
    return this.map;
  }

  /**
   * Gets random key.
   *
   * @return the random key
   */
  public K getRandomKey() {
    int index = random.nextInt(keylist.size());
    return keylist.get(index);
  }

  /**
   * Gets random value.
   *
   * @return the random value
   */
  public V getRandomValue() {
    K key = getRandomKey();
    return map.get(key);
  }

  public int size() {
    return keylist.size();
  }
}
