/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.security.SecureRandom;
import com.ibm.whc.deid.models.FirstName;
import com.ibm.whc.deid.models.Gender;
import com.ibm.whc.deid.models.LastName;
import com.ibm.whc.deid.resources.LocalizedResourceManager;
import com.ibm.whc.deid.shared.localization.Resource;

/**
 * The type Names manager.
 *
 */
public class NamesManager {

  private final SecureRandom random = new SecureRandom();

  private final NameLastManager lastNameManager;
  private final NameFirstMaleManager maleNameManager;
  private final NameFirstFemaleManager femaleNameManager;

  protected NamesManager(NameLastManager lastNameManager, NameFirstMaleManager maleNameManager,
      NameFirstFemaleManager femaleNameManager) {
    this.lastNameManager = lastNameManager;
    this.maleNameManager = maleNameManager;
    this.femaleNameManager = femaleNameManager;
  }

  public static NamesManager buildNamesManager(String tenantId, String localizationProperty) {
    NamesManager mgr = new NamesManager(
        (NameLastManager) ManagerFactory.getInstance().getManager(tenantId, Resource.LAST_NAME,
            null, localizationProperty),
        (NameFirstMaleManager) ManagerFactory.getInstance().getManager(tenantId,
            Resource.FIRST_NAME_MALE, null, localizationProperty),
        (NameFirstFemaleManager) ManagerFactory.getInstance().getManager(tenantId,
            Resource.FIRST_NAME_FEMALE, null, localizationProperty));
    return mgr;
  }

  /**
   * Determine if a name is a known last name.
   *
   * @param candidate the candidate name
   * 
   * @return <i>true</i> if a known last name and <i>false</i> otherwise
   */
  public boolean isLastName(String candidate) {
    return lastNameManager.isValidKey(candidate);
  }

  /**
   * Determine if a name is a known first name.
   *
   * @param candidate the candidate name
   * 
   * @return <i>true</i> if a known first name and <i>false</i> otherwise
   */
  public boolean isFirstName(String candidate) {
    return maleNameManager.isValidKey(candidate) || femaleNameManager.isValidKey(candidate);
  }

  /**
   * Retrieve a last name for the given key.
   *
   * @param identifier the identifier
   * 
   * @return the last name or <i>null</i> if the given name is not a known last name
   */
  public LastName getLastName(String identifier) {
    return lastNameManager.getValue(identifier);
  }

  /**
   * Retrieve a first name for the given key.
   *
   * @param identifier the identifier
   * 
   * @return the first name or <i>null</i> if the given name is not a known first name
   */
  public FirstName getFirstName(String identifier) {
    FirstName firstName = maleNameManager.getValue(identifier);
    if (firstName == null) {
      firstName = femaleNameManager.getValue(identifier);
    }
    return firstName;
  }

  /**
   * Gets a random last name.
   *
   * @return random last name or <i>null</i> if no last names are loaded
   */
  public String getRandomLastName() {
    LastName name = lastNameManager.getRandomValue();
    return name == null ? null : name.getName();
  }

  /**
   * Gets random last name for a given country code.
   *
   * @param countryCode the country code
   * 
   * @return random last name or <i>null</i> if no last names are loaded
   */
  public String getRandomLastName(String countryCode) {
    LastName name = lastNameManager.getRandomValue(countryCode);
    return name == null ? null : name.getName();
  }

  /**
   * Gets random first name with random gender and country.
   *
   * @return random first name or <i>null</i> if no first names are loaded
   */
  public String getRandomFirstName() {
    FirstName name = null;
    boolean coin = random.nextBoolean();
    if (coin) {
      name = maleNameManager.getRandomValue();
      if (name == null) {
        name = femaleNameManager.getRandomValue();
      }
    } else {
      name = femaleNameManager.getRandomValue();
      if (name == null) {
        name = maleNameManager.getRandomValue();
      }
    }
    return name == null ? null : name.getName();
  }

  /**
   * Retrieve a random first name based on gender, country, and whether to allow unisex names.
   *
   * @param gender the gender of the desired name - male or female
   * @param allowUnisex <i>true</i> if a name that is both a male and a female name is acceptable
   *        and <i>false</i> otherwise
   * @param countryCode the country code
   * 
   * @return random first name or <i>null</i> if no first names for the given country and gender are
   *         loaded or none are found that satisfy the requirements
   */
  public String getRandomFirstName(Gender gender, boolean allowUnisex, String countryCode) {
    FirstName name = null;
    LocalizedResourceManager<FirstName> firstManager =
        gender == Gender.MALE ? maleNameManager : femaleNameManager;
    LocalizedResourceManager<FirstName> otherManager =
        gender == Gender.MALE ? femaleNameManager : maleNameManager;
    // need safety check in case all loaded names for the given country and gender
    // are unisex
    for (int i = 0; i < 500; i++) {
      name = firstManager.getRandomValue(countryCode);
      if (name == null || allowUnisex || !otherManager.isValidKey(name.getKey())) {
        break;
      } else {
        // set to null in case loop breaks for number of attempts
        name = null;
      }
    }
    return name == null ? null : name.getName();
  }

  /**
   * Retrieve a random first name for the given country code and whether to allow unisex names.
   *
   * @param allowUnisex <i>true</i> if a name that is both a male and a female name is acceptable
   *        and <i>false</i> otherwise
   * @param countryCode the country code
   * 
   * @return random first name or <i>null</i> if no first names for the given country code are
   *         loaded or none are found that satisfy the requirements
   */
  public String getRandomFirstNameWithoutPreservingGender(boolean allowUnisex, String countryCode) {
    String value = null;
    Gender firstGender = random.nextBoolean() ? Gender.MALE : Gender.FEMALE;
    value = getRandomFirstName(firstGender, allowUnisex, countryCode);
    if (value == null) {
      // no acceptable values, try other gender
      Gender otherGender = firstGender == Gender.MALE ? Gender.FEMALE : Gender.MALE;
      value = getRandomFirstName(otherGender, allowUnisex, countryCode);
    }
    return value;
  }

  /**
   * Determines whether a given first name is a male name, a female name, or can be either male or
   * female.
   *
   * @param candidate the candidate first name
   * 
   * @return The gender - male, female, both. If the name is not recognized, it is considered
   *         female.
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
   * Calculates a first name based on the given identifier, gender, and whether to allow unisex
   * names. While the list of loaded resources is not changed, multiple calls to this method with
   * the same parameters will return the same name.
   *
   * @param gender the gender of the desired name
   * @param allowUnisex <i>true</i> if a name that is both a male name and a female name is
   *        acceptable and <i>false</i> otherwise
   * @param identifier a value from which a name will be calculated
   * 
   * @return a pseudo-randomly generated first name or <i>null</i> if no name can be generated
   */
  public String getPseudoRandomFirstName(Gender gender, boolean allowUnisex, String identifier) {
    if (identifier == null) {
      identifier = "";
    }
    if (allowUnisex || Gender.BOTH == gender || gender == null) {
      if (0 == identifier.hashCode() % 2) {
        String value = maleNameManager.getPseudorandom(identifier);
        return value != null ? value : femaleNameManager.getPseudorandom(identifier);
      }
      String value = femaleNameManager.getPseudorandom(identifier);
      return value != null ? value : maleNameManager.getPseudorandom(identifier);
    } else if (Gender.MALE == gender) {
      return maleNameManager.getPseudorandom(identifier);
    } else {
      // assuming female
      return femaleNameManager.getPseudorandom(identifier);
    }
  }

  /**
   * Calculates a first name based on a given identifier. While the list of loaded resources is not
   * changed, multiple calls to this method with the same identifier will return the same name.
   * 
   * @param identifier a value from which a name will be calculated
   * 
   * @return a pseudo-randomly generated first name or <i>null</i> if no name can be generated
   */
  public String getPseudoRandomFirstName(String identifier) {
    return getPseudoRandomFirstName(null, true, identifier);
  }

  /**
   * Calculates a last name based on a given identifier. While the list of loaded resources is not
   * changed, multiple calls to this method with the same identifier will return the same name.
   * 
   * @param identifier a value from which a name will be calculated
   * 
   * @return a pseudo-randomly generated last name or <i>null</i> if no name can be generated
   */
  public String getPseudoRandomLastName(String identifier) {
    return lastNameManager.getPseudorandom(identifier);
  }
}
