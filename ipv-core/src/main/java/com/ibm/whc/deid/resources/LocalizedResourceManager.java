/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.resources;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import com.ibm.whc.deid.models.LocalizedEntity;

/**
 * @param <K> the type parameter
 */
public abstract class LocalizedResourceManager<K extends Resource> extends ResourceManager<K> { 

  private final HashMap<String, ArrayList<K>> localizedResourceListMap = new HashMap<>();
  private final HashMap<String, HashMap<String,K>> localizedResourceMapMap = new HashMap<>();
  
  protected void add(String localeCode, String key, K resource) {
    ArrayList<K> list = localizedResourceListMap.get(localeCode);
    if (list == null) {
      list = new ArrayList<>();
      localizedResourceListMap.put(localeCode, list);
    }
    list.add(resource);
    HashMap<String,K> map = localizedResourceMapMap.get(localeCode); 
    if (map == null) {
      map = new HashMap<>();
      localizedResourceMapMap.put(localeCode, map);
    }
    map.put(key, resource);
  }

  /**
   * Retrieves all the keys for the given country or language code.
   * @param countryCode
   * @return
   */
  //public
  protected Set<String> getKeys(String countryCode) {
    Set<String> keys = null;
    Map<String,K> map = localizedResourceMapMap.get(countryCode.toLowerCase());
    if (map == null) {
      keys = Collections.emptySet();
    } else {
      keys = map.keySet();
    }
    return keys;
  }

  /**
   * Gets values.
   *
   * @param countryCode the country code
   * @return the values
   */
  //public
  protected List<K> getValues(String countryCode) {
    List<K> list = localizedResourceListMap.get(countryCode.toLowerCase());
    if (list == null) {
      list = Collections.emptyList();
    }
    return list;
  }

  @Override
  //public
  protected String getPseudorandom(String identifier) {
    String element = null;
    String key = identifier.toUpperCase();
    K value = getValue(key);
    if (value instanceof LocalizedEntity) {
      String countryCode = ((LocalizedEntity) value).getNameCountryCode();
      List<K> list = localizedResourceListMap.get(countryCode.toLowerCase());
      if (list != null) {
        element = getPseudorandomElement(list, key);
      }
    }
    if (element == null) {
      element = super.getPseudorandom(identifier);
    }
    return element;
  }

  /**
   * Gets random key.
   *
   * @param countryCode the country code
   * @return the random key
   */
  //public
  protected String getRandomKey(String countryCode) {
    String key = null;
    List<K> list = localizedResourceListMap.get(countryCode.toLowerCase());
    if (list != null) {
      key = getRandomResource(list).getKey();
    }
    return key;
  }

  //public
  protected K getRandomValue(String countryCode) {
    K value = null;
    List<K> list = localizedResourceListMap.get(countryCode.toLowerCase());
    if (list != null) {
      value = getRandomResource(list);
    }
    return value;
  }
  
  //public
  protected boolean isValidKey(String countryCode, String key) {
    boolean valid = false;
    Map<String, K> map = localizedResourceMapMap.get(countryCode.toLowerCase());
    if (map != null) {
     valid = map.containsKey(key.toUpperCase());
    }
    return valid;
  }

  /**
   * Returns the value for the given key for the given country code.
   *
   * @param countryCode a country or language code
   * @param key the key
   * 
   * @return the value or <i>null</i> if no value for the given key is found 
   * for the given country or language code
   */
  //public
  protected K getValue(String countryCode, String key) {
    K value = null;
    Map<String, K> map = localizedResourceMapMap.get(countryCode.toLowerCase());
    if (map != null) {
      value = map.get(key.toUpperCase());
    }
    return value;
  }
}
