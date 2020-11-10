/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.security.SecureRandom;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.FirstName;
import com.ibm.whc.deid.models.Gender;
import com.ibm.whc.deid.models.LastName;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;

/**
 * The type Names manager.
 *
 */
public class NamesManager implements Serializable {
  /** */
  private static final long serialVersionUID = -8184475896771246147L;

  protected abstract static class NameResourceBasedManager<T> extends ResourceBasedManager<T>
      implements Serializable {

    /** */
    private static final long serialVersionUID = -4285989384152952167L;

    protected NameResourceBasedManager(String tenantId, Resource resourceType) {
      super(tenantId, resourceType);
    }

    /**
     * Read CSV files
     *
     * @param entries
     * @param fn function that stores names into HashMap
     * @param index
     */
    protected void readFile(Collection<ResourceEntry> entries, Function<NameCountryModel, Void> fn,
        int index) {
      for (ResourceEntry entry : entries) {
        InputStream inputStream = entry.createStream();
        String countryCode = entry.getCountryCode();

        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {

          for (CSVRecord line : reader) {
            String name = line.get(index);
            NameCountryModel nameCountry = new NameCountryModel();
            nameCountry.setName(name);
            nameCountry.setCountry(countryCode);
            fn.apply(nameCountry);
          }

          inputStream.close();
        } catch (IOException | NullPointerException e) {
          logger.logError(LogCodes.WPH1013E, e);
        }
      }
    }
  }

  protected static class LastNameManager extends NameResourceBasedManager<LastName>
      implements Serializable {

    protected LastNameManager(String tenantId) {
      super(tenantId, Resource.LAST_NAME);
    }

    /** */
    private static final long serialVersionUID = 340645986264058332L;

    @Override
    public Collection<ResourceEntry> getResources() {
      return LocalizationManager.getInstance().getResources(Resource.LAST_NAME);
    }

    /**
     * Get the function that reads last and and stores in the map.
     *
     * @param namesMap
     * @return
     */
    protected Function<NameCountryModel, Void> getReadFunction(
        Map<String, Map<String, LastName>> namesMap) {
      Function<NameCountryModel, Void> fn = nameCountry -> {
        String name = nameCountry.getName();
        String key = nameCountry.getName().toUpperCase();
        String countryCode = nameCountry.getCountry();

        LastName lastName = new LastName(name, countryCode);
        addToMapByLocale(namesMap, countryCode, key, lastName);
        addToMapByLocale(namesMap, getAllCountriesName(), key, lastName);
        return null;
      };
      return fn;
    }

    @Override
    public Map<String, Map<String, LastName>> readResourcesFromFile(
        Collection<ResourceEntry> entries) throws NullPointerException {
      Map<String, Map<String, LastName>> namesMap = new HashMap<>();

      // last name is stored in column 0
      int index = 0;
      readFile(entries, getReadFunction(namesMap), index);

      return namesMap;
    }

    @Override
    public Collection<LastName> getItemList() {
      return getValues();
    }
  }

  protected static class MaleNameManager extends NameResourceBasedManager<FirstName>
      implements Serializable {

    protected MaleNameManager(String tenantId) {
      super(tenantId, Resource.FIRST_NAME_MALE);
    }

    /** */
    private static final long serialVersionUID = 3798419386305168471L;

    @Override
    public Collection<ResourceEntry> getResources() {
      return LocalizationManager.getInstance().getResources(Resource.FIRST_NAME_MALE);
    }

    /**
     * Get the function that reads the name and put into the map.
     *
     * @param namesMap
     * @return
     */
    protected Function<NameCountryModel, Void> getReadFunction(
        Map<String, Map<String, FirstName>> namesMap) {

      Function<NameCountryModel, Void> fn = nameCountry -> {
        String name = nameCountry.getName();
        String key = nameCountry.getName().toUpperCase();
        String countryCode = nameCountry.getCountry();

        FirstName firstName = new FirstName(name, countryCode, Gender.MALE);
        addToMapByLocale(namesMap, countryCode, key, firstName);
        addToMapByLocale(namesMap, getAllCountriesName(), key, firstName);
        return null;
      };
      return fn;
    }

    @Override
    public Map<String, Map<String, FirstName>> readResourcesFromFile(
        Collection<ResourceEntry> entries) {

      Map<String, Map<String, FirstName>> namesMap = new HashMap<>();

      // first name is stored in column 0
      int index = 0;
      readFile(entries, getReadFunction(namesMap), index);

      return namesMap;
    }

    @Override
    public Collection<FirstName> getItemList() {
      return getValues();
    }
  }

  protected static class FemaleNameManager extends NameResourceBasedManager<FirstName>
      implements Serializable {

    protected FemaleNameManager(String tenantId) {
      super(tenantId, Resource.FIRST_NAME_FEMALE);
    }

    /** */
    private static final long serialVersionUID = 4460091056804556534L;

    @Override
    public Collection<ResourceEntry> getResources() {
      return LocalizationManager.getInstance().getResources(Resource.FIRST_NAME_FEMALE);
    }

    /**
     * Get the function that read name and store into the map.
     *
     * @param namesMap
     * @return
     */
    protected Function<NameCountryModel, Void> getReadFunction(
        Map<String, Map<String, FirstName>> namesMap) {
      Function<NameCountryModel, Void> fn = nameCountry -> {
        String name = nameCountry.getName();
        String key = nameCountry.getName().toUpperCase();
        String countryCode = nameCountry.getCountry();

        FirstName firstName = new FirstName(name, countryCode, Gender.FEMALE);
        addToMapByLocale(namesMap, countryCode, key, firstName);
        addToMapByLocale(namesMap, getAllCountriesName(), key, firstName);
        return null;
      };
      return fn;
    }

    @Override
    public Map<String, Map<String, FirstName>> readResourcesFromFile(
        Collection<ResourceEntry> entries) {
      Map<String, Map<String, FirstName>> namesMap = new HashMap<>();

      // first name is stored in column 0
      int index = 0;
      readFile(entries, getReadFunction(namesMap), index);

      return namesMap;
    }

    @Override
    public Collection<FirstName> getItemList() {
      return getValues();
    }
  }

  /** The type Names. */
  public static class NameManager implements Serializable {

    /** */
    private static final long serialVersionUID = -1913711626600968977L;

    protected LastNameManager lastNameManager;
    protected MaleNameManager maleNameManager;
    protected FemaleNameManager femaleNameManager;

    protected final SecureRandom random;

    public NameManager(String tenantId) {
      initializeManagers(tenantId);
      this.random = new SecureRandom();
    }

    protected void initializeManagers(String tenantId) {
      lastNameManager = new LastNameManager(tenantId);
      maleNameManager = new MaleNameManager(tenantId);
      femaleNameManager = new FemaleNameManager(tenantId);
    }

    /**
     * Is last name boolean.
     *
     * @param candidate the candidate
     * @return the boolean
     */
    public boolean isLastName(String candidate) {
      return lastNameManager.isValidKey(candidate);
    }

    /**
     * Is first name boolean.
     *
     * @param candidate the candidate
     * @return the boolean
     */
    public boolean isFirstName(String candidate) {
      return maleNameManager.isValidKey(candidate) || femaleNameManager.isValidKey(candidate);
    }

    /**
     * Gets last name.
     *
     * @param identifier the identifier
     * @return the last name
     */
    public LastName getLastName(String identifier) {
      return lastNameManager.getKey(identifier);
    }

    /**
     * Gets first name.
     *
     * @param identifier the identifier
     * @return the first name
     */
    public FirstName getFirstName(String identifier) {
      FirstName firstName = maleNameManager.getKey(identifier);

      if (firstName != null) {
        return firstName;
      }

      return femaleNameManager.getKey(identifier);
    }

    /**
     * Gets random last name.
     *
     * @return random last name
     */
    public String getRandomLastName() {
      return lastNameManager.getRandomKey();
    }

    /**
     * Gets random last name based on country.
     *
     * @param countryCode the country code
     * @return random last name
     */
    public String getRandomLastName(String countryCode) {
      return lastNameManager.getRandomKey(countryCode);
    }

    /**
     * Gets random first name with random gender and country.
     *
     * @return random first name
     */
    public String getRandomFirstName() {
      boolean coin = random.nextBoolean();

      if (coin) {
        return maleNameManager.getRandomKey();
      } else {
        return femaleNameManager.getRandomKey();
      }
    }

    /**
     * Get random first name with specified gender and country.
     *
     * @param gender either male or female
     * @param countryCode country that name belongs to
     * @return random first name
     */
    private String getRandomFirstName(Gender gender, String countryCode) {
      if (gender == Gender.MALE) {
        return maleNameManager.getRandomKey(countryCode);
      } else {
        return femaleNameManager.getRandomKey(countryCode);
      }
    }

    /**
     * Gets random first name based on gender, country, and whether to allow unisex.
     *
     * @param gender the gender
     * @param allowUnisex to allow unisex
     * @param countryCode the country code
     * @return random first name
     */
    public String getRandomFirstName(Gender gender, boolean allowUnisex, String countryCode) {

      do {
        String name = getRandomFirstName(gender, countryCode);
        if (allowUnisex) {
          return name;
        } else {
          Gender newGender = getGender(name);
          if (newGender == gender) {
            return name;
          }
        }
      } while (true);
    }

    public String getRandomFirstNameWithoutPreservingGender(boolean allowUnisex,
        String countryCode) {
      boolean coin = random.nextBoolean();
      if (coin) {
        return getRandomFirstName(Gender.MALE, allowUnisex, countryCode);
      }
      return getRandomFirstName(Gender.FEMALE, allowUnisex, countryCode);
    }

    /**
     * Gets gender.
     *
     * @param candidate the candidate
     * @return the gender
     */
    public Gender getGender(String candidate) {
      boolean isMale = maleNameManager.isValidKey(candidate);
      boolean isFemale = femaleNameManager.isValidKey(candidate);

      if (isMale && isFemale) {
        return Gender.BOTH;
      }

      if (isMale) {
        return Gender.MALE;
      } else {
        return Gender.FEMALE;
      }
    }

    /**
     * Get pseudo random first name based on the identifier, gender, and whether to allow unisex.
     *
     * @param gender the gender
     * @param allowUnisex to allow unisex
     * @param identifier the identifier
     * @return pseudo random first name
     */
    public String getPseudoRandomFirstName(Gender gender, boolean allowUnisex, String identifier) {
      if (allowUnisex || Gender.BOTH == gender) {
        if (0 == identifier.hashCode() % 2) {
          return maleNameManager.getPseudorandom(identifier);
        } else {
          return femaleNameManager.getPseudorandom(identifier);
        }
      } else if (Gender.MALE == gender) {
        return maleNameManager.getPseudorandom(identifier);
      } else {
        // assuming female
        return femaleNameManager.getPseudorandom(identifier);
      }
    }

    /**
     * Get pseudo random first name based on the identifier.
     *
     * @param identifier the identifier
     * @return pseudo random first name
     */
    public String getPseudoRandomFirstName(String identifier) {
      boolean coin = random.nextBoolean();
      if (coin) {
        return maleNameManager.getPseudorandom(identifier);
      } else {
        return femaleNameManager.getPseudorandom(identifier);
      }
    }

    /**
     * Get pseudo random last name based on the identifier
     *
     * @param identifier the identifier
     * @return pseudo random last name
     */
    public String getPseudoRandomLastName(String identifier) {
      return lastNameManager.getPseudorandom(identifier);
    }

  }
}
