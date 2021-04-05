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
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * A class that provides access to individual items of a particular type recognized by the
 * De-Identification service, such as cities, names, genders, religions, etc.
 */
public abstract class ResourceManager<K extends ManagedResource> implements Manager {

  private static final LogManager logger = LogManager.getInstance();
  
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
    K oldValue = resourceMap.put(resource.getKey().toUpperCase(), resource);
    if (oldValue != null) {
      logger.logWarn(LogCodes.WPH1021W, oldValue.getKey());
    }
  }

  /**
   * Retrieve the keys for all the loaded resources.
   * 
   * @return a non-null, possibly empty set of the keys for each of the resources
   */
  public Set<String> getKeys() {
    return resourceMap.keySet();
  }

  /**
   * Retrieve all the resources.
   *
   * @return a non-null, possibly-empty list of all the loaded resources
   */
  public List<K> getValues() {
    return resourceList;
  }

  /**
   * Returns a value based on a mathematical computation using the given value. As long as the
   * number of loaded resources remains constant, the same replacement value is returned each time
   * the same input (ignoring case) is presented. If resources have been loaded, the returned value
   * is the key of one of the loaded resources. if not, the returned value is generated from the
   * computation performed on the input.
   * 
   * @param identifier input value for which a pseudorandom replacement is required
   * 
   * @return the replacement value, generated as described
   */
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

  /**
   * Retrieves the key from one of the loaded resources selected at random.
   * 
   * @return the key from one of the loaded resources or <i>null</i> if no resources have been
   *         loaded
   */
  @Override
  public String getRandomKey() {
    String key = null;
    ManagedResource resource = getRandomResource(resourceList);
    if (resource != null) {
      key = resource.getKey();
    }
    return key;
  }

  /**
   * Retrieves one of the loaded resources selected at random.
   * 
   * @return one of the loaded resources or <i>null</i> if no resources have been loaded
   */
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

  /**
   * Determines whether the given input is the key (ignoring case) of one of the loaded resources.
   * 
   * @param key the value to test
   * 
   * @return <i>True</i> if the input equals the key, ignoring case, of one of the loaded resources
   *         and <i>False</i> if not
   */
  @Override
  public boolean isValidKey(String key) {
    return key == null ? false : resourceMap.containsKey(key.toUpperCase());
  }

  /**
   * Returns the value for the given key (ignoring case).
   *
   * @param key the key
   * 
   * @return the value or <i>null</i> if no value with the given key has been loaded
   */
  public K getValue(String key) {
    K value = null;
    if (key != null) {
      value = resourceMap.get(key.toUpperCase());
    }
    return value;
  }
}
