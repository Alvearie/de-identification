/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.resources;

import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import com.ibm.whc.deid.util.HashUtils;
import com.ibm.whc.deid.util.Manager;

/**
 * A class that provides access to individual items of a particular type recognized by the
 * De-Identification service, such as cities, names, genders, religions, etc.
 */
public abstract class ResourceManager<K extends ManagedResource> implements Manager {

  protected final SecureRandom random = new SecureRandom();

  /**
   * The known items stored in a list
   */
  private final ArrayList<K> resourceList = new ArrayList<>();

  /**
   * The known items/resources stored as a map from the resource key to the resource/item itself
   */
  private final HashMap<String, K> resourceMap = new HashMap<>();

  /**
   * Adds a new resource to the manager instance.
   * 
   * <p>
   * This method should only be called during instance construction.
   * 
   * @param resource the resource being added
   */
  protected void add(K resource) {
    resourceList.add(resource);
    resourceMap.put(resource.getKey(), resource);
  }

  public Set<String> getKeys() {
    return resourceMap.keySet();
  }

  /**
   * Gets values.
   *
   * @return the values
   */
  public List<K> getValues() {
    return resourceList;
  }

  public String getPseudorandom(String identifier) {
    return getPseudorandomElement(resourceList, identifier == null ? "" : identifier.toUpperCase());
  }

  protected String getPseudorandomElement(List<K> resources, String key) {
    String element;
    long hash = Math.abs(HashUtils.longFromHash(key));
    int count = resources.size();
    if (count == 0) {
      element = Long.toString(hash);
    } else {
      int position = (int) (hash % count);
      element = resources.get(position).getKey();
    }
    return element;
  }

  @Override
  public String getRandomKey() {
    String key = null;
    ManagedResource resource = getRandomResource(resourceList);
    if (resource != null) {
      key = resource.getKey();
    }
    return key;
  }

  public K getRandomValue() {
    return getRandomResource(resourceList);
  }

  protected K getRandomResource(List<K> resources) {
    K resource = null;
    int count = resources.size();
    if (count == 1) {
      resource = resources.get(0);
    } else if (count > 1) {
      int index = random.nextInt(count);
      resource = resources.get(index);
    }
    return resource;
  }

  @Override
  public boolean isValidKey(String key) {
    return key == null ? false : resourceMap.containsKey(key.toUpperCase());
  }

  /**
   * Returns the value for the given key regardless of country code.
   *
   * @param key the key
   * 
   * @return the value or <i>null</i> if no value for the given key is found
   */
  public K getValue(String key) {
    K value = null;
    if (key != null) {
      value = resourceMap.get(key.toUpperCase());
    }
    return value;
  }
}
