/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util.localization;

import java.io.FileNotFoundException;
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
import java.util.concurrent.ConcurrentHashMap;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * Class that handles loading resource instances of various types from CSV files. There is one
 * instance of this class per resource type.
 * 
 * <p>
 * This class must remain thread-safe.
 */
public class LocalizationManager {
  private static final String COMMON = "_common";
  private static final LogManager logger = LogManager.getInstance();

  // A common set of enabled countries among all source properties files processed so far
  private static final Set<String> enabledCountries = ConcurrentHashMap.newKeySet();

  private final Map<Resources, Map<String, ResourceEntry>> registeredResources;
  private final Map<String, String> countryCommonMap;
  private final Map<String, Properties> countryLocalizationOptions;

  public static final String DEFAULT_LOCALIZATION_PROPERTIES = "/localization.properties";

  /**
   * A map from initialization properties files paths (keys) to the LocalizationManager instances
   * built from each (values).
   */
  private static final Map<String, LocalizationManager> localizationManagers =
      new ConcurrentHashMap<>();

  /**
   * Gets a LocalizationManager based on the given properties file.
   * 
   * @param propertyFile Location of the properties file from which a LocalizableManager instance is
   *        to be built.
   *
   * @return the LocalizableManager, either just built or retrieved from the cache
   */
  public static LocalizationManager getInstance(String propertyFile) {
    LocalizationManager manager = localizationManagers.get(propertyFile);
    if (manager == null) {
      synchronized (localizationManagers) {
        manager = localizationManagers.get(propertyFile);
        if (manager == null) {
          manager = new LocalizationManager(propertyFile);
          localizationManagers.put(propertyFile, manager);
        }
      }
    }
    return manager;
  }

  /**
   * Instantiates a new Localization manager based on the properties file at the given location.
   * 
   * @param propertyFile Location of the properties file from which a LocalizableManager instance is
   *        to be built.
   */
  private LocalizationManager(String propertyFile) {
    this.registeredResources = new ConcurrentHashMap<>();
    // regular HashMap appropriate - all structural changes are here during construction
    this.countryCommonMap = new HashMap<>();
    // regular HashMap appropriate - all structural changes are here during construction
    this.countryLocalizationOptions = new HashMap<>();

    try (InputStream is = getClass().getResourceAsStream(propertyFile)) {
      if (is == null) {
        throw new FileNotFoundException(propertyFile);
      }

      Properties properties = new Properties();
      properties.load(is);

      // load enabledCountries
      for (final String country : properties.getProperty("country").split(",")) {
        // logger.debug("Enabling country {}", country);
        enabledCountries.add(country.trim());

        // Get any locale properties for this country
        try (InputStream countryOptionStream = getClass().getResourceAsStream(
            "/identifier/" + country.trim().toLowerCase() + "/locale.properties")) {
          // the supplemental properties are not required for each country
          if (null != countryOptionStream) {
            Properties localeProperties = new Properties();
            localeProperties.load(countryOptionStream);
            countryLocalizationOptions.put(country.trim().toLowerCase(), localeProperties);
          }
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

      for (final Resources resource : Resource.values()) {
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

        for (final String country : countryCommonMap.values()) {
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

      logger.logInfo(LogCodes.WPH1009I, enabledCountries.size(), countryCommonMap.size(),
          registeredResources.size());
    } catch (IOException e) {
      logger.logError(LogCodes.WPH1013E, e);
      throw new RuntimeException(e);
    }
  }

  /**
   * Register a single resource type described in a property file
   *
   * @param localizationProps The list of available resources per country
   * @param resource The resource type to load
   */
  public void registerResourceForSupportedCountries(InputStream localizationProps,
      Resources resource) {
    try {
      Properties properties = new Properties();
      properties.load(localizationProps);

      // Load resource if it has a definition for the given country
      for (final String country : enabledCountries) {
        final String path = properties.getProperty(country + '.' + resource.name());
        if (null != path) {
          registerResource(resource, country, path);
        }
      }

      // Load resource if it has a definition for the given country
      for (final String country : countryCommonMap.values()) {
        final String path = properties.getProperty(country + '.' + resource.name());
        if (null != path) {
          registerResource(resource, country, path);
        }
      }

      // Load resource if it has a common definition
      final String path = properties.getProperty(COMMON + '.' + resource.name());
      if (null != path) {
        registerResource(resource, COMMON, path);
      }
    } catch (IOException e) {
      logger.logError(LogCodes.WPH1013E, e);
      throw new RuntimeException(e);
    }
  }

  private void registerResource(Resources resource, ResourceEntry entry) {
    Map<String, ResourceEntry> entries = this.registeredResources.get(resource);
    if (entries == null) {
      synchronized (this.registeredResources) {
        entries = this.registeredResources.get(resource);
        if (entries == null) {
          entries = new ConcurrentHashMap<String, ResourceEntry>();
          this.registeredResources.put(resource, entries);
        }
      }
    }
    entries.put(entry.getCountryCode(), entry);
  }

  private void registerResource(Resources resource, String countryCode, String filename) {
    registerResource(resource,
        new ResourceEntry(filename, countryCode, ResourceEntryType.EXTERNAL_FILENAME));
  }

  /**
   * Gets records describing resources files for the given resource type for any of the countries in
   * the given list of countries.
   *
   * @param resource the type of resource for which resources are to be obtained
   * @param countries the countries for which resources are to be obtained
   * 
   * @return a possibly-empty collection of records describing the available resources
   */
  public Collection<ResourceEntry> getResources(Resources resource, Collection<String> countries) {
    // logger.debug("Requesting {} for {}", resource,
    // Arrays.toString(countries.toArray()));

    final List<ResourceEntry> entries = new ArrayList<>();
    final Set<String> countriesLoaded = new HashSet<>();

    final Map<String, ResourceEntry> knownEntries = registeredResources.get(resource);

    if (null == knownEntries) {
      return entries;
    }

    if (resource instanceof Resource) {
      switch ((Resource) resource) {
        case ATC_CODES:
        case TACDB:
        case PUBLIC_SUFFIX_LIST:
        case GENERALIZE:
        case PHONE_NUM_DIGITS:
          return Collections.singletonList(knownEntries.get(COMMON));

        default:
          // fall through and let the following code handle it.
          break;
      }
    } // else go through code below as generic Resources

    for (String country : countries) {
      // logger.debug("Retrieving country {}", country);

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
  public Collection<ResourceEntry> getResources(Resources resource) {
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
    Properties p = null;
    if (country != null) {
      p = this.countryLocalizationOptions.get(country.toLowerCase().trim());
    }
    if (p == null) {
      p = new Properties();
    }
    return p;
  }
}
