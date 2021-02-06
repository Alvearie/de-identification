/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util.localization;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class LocalizationManager {
  private static final String COMMON = "_common";
  private static LogManager logger = LogManager.getInstance();

  /** The constant enabledCountries. */
  private static Collection<String> enabledCountries = new ArrayList<>();

  private final Map<Resource, Map<String, ResourceEntry>> registeredResources;
  private final Map<String, String> countryCommonMap;
  private final Map<String, Properties> countryLocalizationOptions;

  private static final LocalizationManager instance = new LocalizationManager();

  /**
   * Gets instance.
   *
   * @return the instance
   */
  public static LocalizationManager getInstance() {
    return instance;
  }

  /** Instantiates a new Localization manager. */
  private LocalizationManager() {
    this.registeredResources = new HashMap<>();
    this.countryCommonMap = new HashMap<>();
    this.countryLocalizationOptions = new HashMap<>();

    try (InputStream is = getClass().getResourceAsStream("/localization.properties")) {
      if (null != is) {
        Properties properties = new Properties();
        properties.load(is);

        // load enabledCountries
        for (final String country : properties.getProperty("country").split(",")) {
          // logger.debug("Enabling country {}", country);
          if (!enabledCountries.contains(country.trim()))
            enabledCountries.add(country.trim());

          // Get any locale properties for this country
          try (InputStream countryOptionStream = getClass().getResourceAsStream(
              "/identifier/" + country.trim().toLowerCase() + "/locale.properties")) {
            if (null != countryOptionStream) {
              Properties localeProperties = new Properties();
              localeProperties.load(countryOptionStream);
              countryLocalizationOptions.put(country.trim().toLowerCase(), localeProperties);
            }
          } catch (IOException e) {
            logger.logError(LogCodes.WPH1013E, e);
          }
        }

        // common map
        for (final String country : enabledCountries) {
          // logger.debug("Verifying mapping for {}", country);
          final String language = properties.getProperty(country);

          if (null != language) {
            // logger.info("Adding mapping for {}, {}", country,
            // language);
            countryCommonMap.put(country, language);
          }
        }

        for (final Resource resource : Resource.values()) {
          // logger.debug("Adding localization for {}", resource);

          // initialize resources
          for (final String country : enabledCountries) {
            final String path = properties.getProperty(country + '.' + resource.name());
            if (null != path) {
              // logger.debug("Creating resources for country {}",
              // country);
              registerResource(resource,
                  new ResourceEntry(path, country, ResourceEntryType.INTERNAL_RESOURCE));
            }
          }

          for (final String country : new HashSet<>(countryCommonMap.values())) {
            final String path = properties.getProperty(country + '.' + resource.name());
            if (null != path) {
              // logger.debug("Creating resources for language
              // {}",
              // country);
              registerResource(resource,
                  new ResourceEntry(path, country, ResourceEntryType.INTERNAL_RESOURCE));
            }
          }

          final String path = properties.getProperty(COMMON + '.' + resource.name());
          if (null != path) {
            // logger.debug("Creating resources as common");
            registerResource(resource,
                new ResourceEntry(path, COMMON, ResourceEntryType.INTERNAL_RESOURCE));
          }
        }
      }

      //
      try (InputStream external =
          this.getClass().getResourceAsStream("/localization_external.properties")) {
      }

      logger.logInfo(LogCodes.WPH1009I, enabledCountries.size(), countryCommonMap.size(),
          registeredResources.size());
    } catch (IOException e) {
      logger.logError(LogCodes.WPH1013E, e);
    }
  }

  /**
   * Register an advanced custom resource
   *
   * @param cl The class loader of the origin package
   * @param resource The resource type to load
   * @param localizationProperties The list of available resources per country
   * @return the boolean
   */
  public synchronized void registerResourceForSupportedCountries(Manager manager,
      Resource resource, String localizationProperties) {

    try (InputStream is = manager.getClass().getResourceAsStream(localizationProperties)) {
      if (null != is) {
        Properties properties = new Properties();
        properties.load(is);

        // Load resource if it has a definition for the given country
        for (final String country : enabledCountries) {
          final String path = properties.getProperty(country + '.' + resource.name());
          if (null != path) {
            registerResource(Resource.DEPENDENT, country,
                manager.getClass().getResource(path).getFile());
          }
        }

        // Load resource if it has a definition for the given country
        for (final String country : new HashSet<>(countryCommonMap.values())) {
          final String path = properties.getProperty(country + '.' + resource.name());
          if (null != path) {
            registerResource(Resource.DEPENDENT, country,
                manager.getClass().getResource(path).getFile());
          }
        }

        // Load resource if it has a common definition
        final String path = properties.getProperty(COMMON + '.' + resource.name());
        if (null != path) {
          registerResource(Resource.DEPENDENT, COMMON,
              manager.getClass().getResource(path).getFile());
        }
      }
    } catch (IOException e) {
      logger.logError(LogCodes.WPH1013E, e);
    }
  }

  private synchronized boolean registerResource(Resource resource, ResourceEntry entry) {
    Map<String, ResourceEntry> entries = this.registeredResources.get(resource);

    if (entries == null) {
      this.registeredResources.put(resource, new HashMap<String, ResourceEntry>());
      entries = this.registeredResources.get(resource);
    }

    entries.put(entry.getCountryCode(), entry);

    return true;
  }

  /**
   * Register resource boolean.
   *
   * @param resource the resource
   * @param countryCode the country code
   * @param filename the filename
   * @return the boolean
   */
  public synchronized boolean registerResource(Resource resource, String countryCode,
      String filename) {
    return registerResource(resource,
        new ResourceEntry(filename, countryCode, ResourceEntryType.EXTERNAL_FILENAME));
  }

  /**
   * Gets resources.
   *
   * @param resource the resource
   * @param countries the countries
   * @return the resources
   */
  public Collection<ResourceEntry> getResources(Resource resource, Collection<String> countries) {
    // logger.debug("Requesting {} for {}", resource,
    // Arrays.toString(countries.toArray()));

    final List<ResourceEntry> entries = new ArrayList<>();
    final Set<String> countriesLoaded = new HashSet<>();

    final Map<String, ResourceEntry> knownEntries = registeredResources.get(resource);

    if (null == knownEntries)
      return entries;

    switch (resource) {
      case ATC_CODES:
      case TACDB:
      case PUBLIC_SUFFIX_LIST:
      case GENERALIZE:
        return Collections.singletonList(knownEntries.get(COMMON));
      case PHONE_NUM_DIGITS:
        return Collections.singletonList(knownEntries.get(COMMON));

      default:
        // fall through and let the following code handle it.
        break;
    }

    for (String country : countries) {
      // logger.debug("Retriving country {}", country);

      if (!knownEntries.containsKey(country)) {
        // logger.debug("Country is not known");
        final String mapped = countryCommonMap.get(country);

        if (mapped != null) {
          // logger.debug("Mapping is known: {} -> {}", country,
          // mapped);
          country = mapped;
        } else {
          // logger.debug("No known mapping for {}", country);
          continue;
        }
      } else {
        // logger.debug("{} is among the known localization for {}",
        // country, resource);
      }

      if (countriesLoaded.contains(country)) {
        // logger.debug("{} was already processed, continuing",
        // country);
        continue;
      }
      final ResourceEntry entry = knownEntries.get(country);

      if (null == entry) {
        // logger.debug("{} is unknown for {}", country, resource);
        continue;
      }
      entries.add(entry);
      countriesLoaded.add(country);
    }

    return entries;
  }

  /**
   * Gets resources.
   *
   * @param resource the resource
   * @return the resources
   */
  public Collection<ResourceEntry> getResources(Resource resource) {
    return getResources(resource, enabledCountries);
  }

  /**
   * Get the locale properties given a country code.
   *
   * @param country The ISO 2 character country code to get the properties for
   * @return The Properties object representing a locale's options. If the file is not present for
   *         this country, an empty properties object is returned in order to make calls to obtain
   *         locale information consistent whether the properties file is not present for the given
   *         country, or the country doesn't exist at all
   */
  public Properties getLocaleProperties(String country) {
    if (country == null) {
      return new Properties();
    }

    Properties p = this.countryLocalizationOptions.get(country.toLowerCase().trim());
    if (p == null) {
      return new Properties();
    }
    return p;
  }
}
