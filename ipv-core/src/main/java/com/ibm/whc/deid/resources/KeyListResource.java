/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.resources;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

/**
 * A resource consisting of a key and a list of items related to that key.
 */
public class KeyListResource<K> implements Serializable, ManagedResource {

  private static final long serialVersionUID = -5306850639448831252L;
  
  private final String key;
  private final List<K> value;

  /**
   * Instantiates a new resource object.
   *
   * @param key the unique key for this resource
   * 
   * @param value The values associated with the given key. Null values are ignored. A copy of the
   *        given list is made so future changes to the given list are not included in this object.
   *        The items in the collection are expected to be immutable.
   * 
   * @throws IllegalArgumentException if the given key is null or whitespace or the given collection
   *         is null or has no non-null members.
   */
  public KeyListResource(String key, Collection<K> values) {
    if (key == null || key.trim().isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(key), "list resource key"));
    }
    if (values == null) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(values), "list resource value"));
    }
    this.key = key;    
    this.value = new ArrayList<>(values.size());
    for (K item : values) {
      if (item != null) {
        this.value.add(item);
      }
    }
    if (this.value.isEmpty()) {
      throw new IllegalArgumentException(
          Messages.getMessage(LogCodes.WPH1010E, String.valueOf(values), "list resource value"));
    }
  }

  /**
   *
   * @return the value
   */
  public List<K> getValue() {
    return value;
  }

  @Override
  public String getKey() {
    return key;
  }
}
