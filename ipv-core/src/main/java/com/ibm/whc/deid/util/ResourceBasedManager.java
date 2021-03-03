/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import com.ibm.whc.deid.models.LocalizedEntity;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * @param <K> the type parameter
 */
public abstract class ResourceBasedManager<K> extends AbstractManager<K> implements Serializable {

  private static final long serialVersionUID = -2677416081691708110L;

  private static final String allCountriesName = "__all__";
  private final Map<String, MapWithRandomPick<String, K>> resourceMap;
  private final Map<String, List<String>> listMap;

  protected final String tenantId;

  protected final Resources resourceType;

  static final LogManager logger = LogManager.getInstance();

  protected int resourceInDbCount = 0;

  protected final String localizationProperty;


  /**
   * Gets all countries name.
   *
   * @return the all countries name
   */
  protected final String getAllCountriesName() {
    return ResourceBasedManager.allCountriesName;
  }

  /**
   * Add to map by locale.
   *
   * @param perLocaleMap the per locale map
   * @param countryCode the country code
   * @param key the key
   * @param value the value
   */
  protected void addToMapByLocale(Map<String, Map<String, K>> perLocaleMap, String countryCode,
      String key, K value) {
    Map<String, K> localMap = perLocaleMap.get(countryCode);

    if (localMap == null) {
      localMap = new HashMap<String, K>();
      perLocaleMap.put(countryCode, localMap);
      listMap.put(countryCode, new ArrayList<String>());
    }

    localMap.put(key, value);
    listMap.get(countryCode).add(key);
  }

  /**
   * Gets resource filenames.
   *
   * @return the resource filenames
   */
  protected abstract Collection<ResourceEntry> getResources();

  /**
   * Read resource file map.
   *
   * @param streams the input streams
   * @return the map
   */
  protected abstract Map<String, Map<String, K>> readResourcesFromFile(
      Collection<ResourceEntry> streams);

  /** Init. */
  protected void init() {}

  /** Post init. */
  protected void postInit() {}

  /**
   * Gets separator.
   *
   * @return the separator
   */
  protected char getSeparator() {
    return ',';
  }

  /**
   * Gets quote char.
   *
   * @return the quote char
   */
  protected char getQuoteChar() {
    return '"';
  }

  /**
   * Instantiates a new Resource based manager.
   *
   * @param tenantId tenant id
   * @param localizationProperty TODO
   */
  public ResourceBasedManager(String tenantId, Resources resourceType,
      String localizationProperty) {
    this.tenantId = tenantId;
    this.localizationProperty = localizationProperty;
    this.resourceType = resourceType;
    init();

    this.resourceMap = new HashMap<>();
    this.listMap = new HashMap<>();
    Map<String, Map<String, K>> contents = readResources(resourceType, tenantId);

    for (final Map.Entry<String, Map<String, K>> entry : contents.entrySet()) {
      final String countryCode = entry.getKey();
      final Map<String, K> perCountryData = entry.getValue();
      MapWithRandomPick<String, K> mapWithRandomPick = new MapWithRandomPick<>(perCountryData);
      this.resourceMap.put(countryCode, mapWithRandomPick);
      this.resourceMap.get(countryCode).setKeyList();
    }

    postInit();
  }

  protected Map<String, Map<String, K>> readResources(Resources resourceType, String tenantId) {
    return readResourcesFromFile(getResources());
  }

  /**
   * Gets values.
   *
   * @return the values
   */
  public Collection<K> getValues() {
    return getValues(allCountriesName);
  }

  /**
   * Gets values.
   *
   * @param countryCode the country code
   * @return the values
   */
  public Collection<K> getValues(String countryCode) {
    MapWithRandomPick<String, K> map = resourceMap.get(countryCode);
    if (map != null) {
      return map.getMap().values();
    }
    if (allCountriesName.equals(countryCode)) {
      return null;
    }    
    return getValues(allCountriesName);
  }

  public List<String> getKeys() {
    return listMap.get(allCountriesName);
  }

  public List<String> getKeys(String countryCode) {
    List<String> list = listMap.get(countryCode);
    if (list != null) {
      return list;
    }

    return listMap.get(allCountriesName);
  }

  private String getPseudorandomElement(List<String> keys, String key) {
    Long hash = Math.abs(HashUtils.longFromHash(key, "SHA-256"));

    if (keys == null || keys.size() == 0) {
      return hash.toString();
    }

    int position = (int) (hash % keys.size());
    return keys.get(position);
  }

  public String getPseudorandom(String identifier) {
    String key = identifier.toUpperCase();
    K value = getKey(key);

    if (value == null) {
      return getPseudorandomElement(getKeys(), key);
    } else {
      if (value instanceof LocalizedEntity) {
        String countryCode = ((LocalizedEntity) value).getNameCountryCode();
        return getPseudorandomElement(getKeys(countryCode), key);
      }

      return getPseudorandomElement(getKeys(), key);
    }
  }

  @Override
  public String getRandomKey() {
    return getRandomKey(allCountriesName);
  }

  public K getRandomValue() {
    return getRandomValue(allCountriesName);
  }

  public K getRandomValue(String countryCode) {
    K value = null;
    MapWithRandomPick<String, K> map = resourceMap.get(countryCode);
    if (map != null && map.size() > 0) {
      value = map.getRandomValue();
    }
    return value;
  }

  /**
   * Gets random key.
   *
   * @param countryCode the country code
   * @return the random key
   */
  public String getRandomKey(String countryCode) {
    MapWithRandomPick<String, K> map = resourceMap.get(countryCode);
    if (map != null) {
      return map.getRandomKey();
    }
    return null;
  }

  @Override
  public boolean isValidKey(String key) {
    return isValidKey(allCountriesName, key);
  }

  public boolean isValidKey(String countryCode, String key) {
    MapWithRandomPick<String, K> map = resourceMap.get(countryCode.toLowerCase());
    return map != null && map.getMap().containsKey(key.toUpperCase());
  }

  /**
   * Returns the value for the given key regardless of country code.
   *
   * @param key the key
   * 
   * @return the value or <i>null</i> if no value for the given key is found 
   */
  public K getKey(String key) {
    return getKey(allCountriesName, key);
  }

  /**
   * Returns the value for the given key for the given country code.
   *
   * @param countryCode a country or language code
   * @param key the key
   * 
   * @return the value or <i>null</i> if no value for the given key is found 
   */
  public K getKey(String countryCode, String key) {
    MapWithRandomPick<String, K> map = resourceMap.get(countryCode.toLowerCase());

    if (map != null) {
      return map.getMap().get(key.toUpperCase());
    }

    return null;
  }

}
