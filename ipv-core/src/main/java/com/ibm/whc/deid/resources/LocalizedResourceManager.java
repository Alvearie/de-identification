/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.resources;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import com.ibm.whc.deid.models.LocalizedEntity;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * Extension of the ResourceManager class that further manages resources by locale.
 */
public abstract class LocalizedResourceManager<K extends ManagedResource>
    extends ResourceManager<K> {

  private static final LogManager logger = LogManager.getInstance();
  
  private final int expectedCountPerLocale;

  /**
   * A map of a locale identifier (key) to the list (value) of resources related to that locale
   */
  private final HashMap<String, ArrayList<K>> localizedResourceListMap = new HashMap<>();

  /**
   * A map of a locale identifier (key) to a map (value) of resources related to that locale mapped
   * by the resource key
   */
  private final HashMap<String, HashMap<String, K>> localizedResourceMapMap = new HashMap<>();


  public LocalizedResourceManager() {
    this(-1);
  }

  public LocalizedResourceManager(int expectedCount) {
    this(expectedCount, -1);
  }

  public LocalizedResourceManager(int expectedCount, int perLocaleExpectedCount) {
    super(expectedCount);
    this.expectedCountPerLocale = perLocaleExpectedCount < 1 ? 16 : perLocaleExpectedCount;
  }

  /**
   * Adds a new resource associated with the given locale to the manager instance.
   * 
   * <p>
   * This method should only be called during instance construction.
   * 
   * @param localeCode the locale
   * @param resource the resource being added
   */
  protected void add(String localeCode, K resource) {
    String lcode = localeCode.toLowerCase();
    ArrayList<K> list = localizedResourceListMap.get(lcode);
    if (list == null) {
      list = new ArrayList<>(expectedCountPerLocale);
      localizedResourceListMap.put(lcode, list);
    }
    list.add(resource);
    HashMap<String, K> map = localizedResourceMapMap.get(lcode);
    if (map == null) {
      map = new HashMap<>(Math.round(expectedCountPerLocale / 0.75f) + 1, 0.75f);
      localizedResourceMapMap.put(lcode, map);
    }
    K oldValue = map.put(resource.getKey().toUpperCase(), resource);
    if (oldValue != null) {
      logger.logWarn(LogCodes.WPH1022W, localeCode, oldValue.getKey());
    }
  }

  /**
   * Retrieves the keys for all the resources associated with the given country or language code.
   * 
   * @param countryCode the localization code
   * 
   * @return a non-null, possibly empty set of the keys for each of the resources
   */
  public Set<String> getKeys(String countryCode) {
    Set<String> keys = null;
    Map<String, K> map = null;
    if (countryCode != null) {
      map = localizedResourceMapMap.get(countryCode.toLowerCase());
    }
    if (map == null) {
      keys = Collections.emptySet();
    } else {
      keys = map.keySet();
    }
    return keys;
  }

  /**
   * Retrieves the all resources associated with the given country or language code.
   *
   * @param countryCode the localization code
   * 
   * @return the non-null, possibly-empty list of resources
   */
  public List<K> getValues(String countryCode) {
    List<K> list = null;
    if (countryCode != null) {
      list = localizedResourceListMap.get(countryCode.toLowerCase());
    }
    if (list == null) {
      list = Collections.emptyList();
    }
    return list;
  }

  /**
   * Returns a value based on a mathematical computation using the given value. As long as the
   * number of loaded resources remains constant, the same replacement value is returned each time
   * the same input (ignoring case) is presented. If resources have been loaded, the returned value
   * is the key of one of the loaded resources. if not, the returned value is generated from the
   * computation performed on the input.
   * 
   * <p>
   * If the input value matches, ignoring case, the key of a loaded resource and if that resource is
   * associated with a country or locale, the returned value will be the key of one of the resources
   * associated with that same country or locale. Otherwise, a value is returned as described for
   * the superclass.
   * 
   * @param identifier input value for which a pseudorandom replacement is required
   * 
   * @return the replacement value, generated as described
   */
  @Override
  public String getPseudorandom(String identifier) {
    String element = null;
    String key = identifier == null ? "" : identifier.toUpperCase();
    K value = getValue(key);
    if (value instanceof LocalizedEntity) {
      String countryCode = ((LocalizedEntity) value).getNameCountryCode();
      List<K> list = null;
      if (countryCode != null) {
        list = localizedResourceListMap.get(countryCode.toLowerCase());
      }
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
   * Retrieves the key from one of the loaded resources associated with the given localization code.
   * 
   * @param countryCode the localization code
   * 
   * @return the key from one of the loaded resources or <i>null</i> if no resources have been
   *         loaded for the given localization code.
   */
  public String getRandomKey(String countryCode) {
    String key = null;
    if (countryCode != null) {
      List<K> list = localizedResourceListMap.get(countryCode.toLowerCase());
      if (list != null) {
        key = getRandomResource(list).getKey();
      }
    }
    return key;
  }

  /**
   * Retrieves one of the loaded resources associated with the given localization code.
   * 
   * @param countryCode the localization code
   * 
   * @return one of the loaded resources or <i>null</i> if no resources have been loaded for the
   *         given localization code.
   */
  public K getRandomValue(String countryCode) {
    K value = null;
    if (countryCode != null) {
      List<K> list = localizedResourceListMap.get(countryCode.toLowerCase());
      if (list != null) {
        value = getRandomResource(list);
      }
    }
    return value;
  }

  /**
   * Determines whether the given input is the key (ignoring case) of one of the loaded resources
   * associated with the given localization code.
   * 
   * @param key the value to test
   * @param countryCode the localization code
   * 
   * @return <i>True</i> if the input equals the key, ignoring case, of one of the loaded resources
   *         associated with the given localization code and <i>False</i> if not
   */
  public boolean isValidKey(String countryCode, String key) {
    boolean valid = false;
    if (countryCode != null && key != null) {
      Map<String, K> map = localizedResourceMapMap.get(countryCode.toLowerCase());
      if (map != null) {
        valid = map.containsKey(key.toUpperCase());
      }
    }
    return valid;
  }

  /**
   * Returns the value for the given key for the given localization code.
   *
   * @param countryCode the localization code
   * @param key the key
   * 
   * @return the associated that has the given key or <i>null</i> if no value has been loaded that
   *         is associated with the given localization code and has the given key
   */
  public K getValue(String countryCode, String key) {
    K value = null;
    if (countryCode != null && key != null) {
      Map<String, K> map = localizedResourceMapMap.get(countryCode.toLowerCase());
      if (map != null) {
        value = map.get(key.toUpperCase());
      }
    }
    return value;
  }
}
