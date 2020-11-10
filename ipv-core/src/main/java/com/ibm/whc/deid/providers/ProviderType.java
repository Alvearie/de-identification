/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnore;

/** The type Provider type. */
public final class ProviderType implements Serializable {
  /** */
  private static final long serialVersionUID = -2274165028703699686L;

  private static Map<String, ProviderType> registeredTypes = new HashMap<>();

  /** The constant NUMERIC. */
  public static final ProviderType NUMERIC =
      new ProviderType("NUMERIC", "Numeric", "Masks numerical values", TypeClass.NUMERICAL);
  /** The constant NAME. */
  public static final ProviderType NAME =
      new ProviderType("NAME", "Name", "Masks names (with gender preservation) and surnames");
  /** The constant EMAIL. */
  public static final ProviderType EMAIL = new ProviderType("EMAIL", "E-mail",
      "Masks e-mail addresses, supports preservation of domain names");
  /** The constant CREDIT_CARD. */
  public static final ProviderType CREDIT_CARD = new ProviderType("CREDIT_CARD", "Credit Card",
      "Masks credit cards, support vendor preservation");
  /** The constant ADDRESS. */
  public static final ProviderType ADDRESS = new ProviderType("ADDRESS", "Addresses",
      "Masks addresses, supports free text and PO BOX formats");
  /** The constant NATIONAL_ID. */
  public static final ProviderType NATIONAL_ID =
      new ProviderType("NATIONAL_ID", "National ID", "Masks national ID");
  /** The constant IP_ADDRESS. */
  public static final ProviderType IP_ADDRESS = new ProviderType("IP_ADDRESS", "IP address",
      "Masks IP addresses, supports prefix preservation");
  /** The constant URL. */
  public static final ProviderType URL = new ProviderType("URL", "URLs",
      "Masks URLs, supports preservation of domain names and usernames");
  /** The constant VIN. */
  public static final ProviderType VIN = new ProviderType("VIN", "Vehicle Identification Number",
      "Masks VINs, support vendor preservation");
  /** The constant LATITUDE_LONGITUDE. */
  public static final ProviderType LATITUDE_LONGITUDE = new ProviderType("LATITUDE_LONGITUDE",
      "Latitude/Longitude", "Masks latitude/longitude pairs, supports multiple coordinate formats",
      TypeClass.NUMERICAL);
  /** The constant COUNTRY. */
  public static final ProviderType COUNTRY = new ProviderType("COUNTRY", "Country",
      "Masks country names, supports masking based on closest countries");
  /** The constant DATETIME. */
  public static final ProviderType DATETIME = new ProviderType("DATETIME", "Date/Time",
      "Masks date/time, supports multiple datetime formats or user-specified ones",
      TypeClass.NUMERICAL);
  /** The constant CITY. */
  public static final ProviderType CITY =
      new ProviderType("CITY", "City", "Masks city names, support masking based on closest cities");
  /** The constant CONTINENT. */
  public static final ProviderType CONTINENT =
      new ProviderType("CONTINENT", "Continent", "Masks continent names");
  /** The constant ICDv9. */
  public static final ProviderType ICDv9 = new ProviderType("ICDV9", "ICDv9",
      "Masks ICD v9 codes, supports preservation of chapter and section");
  /** The constant ICDv10. */
  public static final ProviderType ICDv10 = new ProviderType("ICDV10", "ICDv10",
      "Masks ICD v10 codes, supports preservation of chapter and section");
  /** The constant PHONE. */
  public static final ProviderType PHONE = new ProviderType("PHONE", "Telephone numbers",
      "Masks phone numbers, supports preservation of country codes and areas");
  /** The constant HOSPITAL. */
  public static final ProviderType HOSPITAL = new ProviderType("HOSPITAL",
      "Hospitals and medical centers", "Masks names of hospitals and medical centers");
  /** The constant RANDOM. */
  public static final ProviderType RANDOM =
      new ProviderType("RANDOM", "Random", "Changes characters of the value randomly");
  /** The constant RELIGION. */
  public static final ProviderType RELIGION =
      new ProviderType("RELIGION", "Religion", "Masks religion names");
  /** The constant MARITAL_STATUS. */
  public static final ProviderType MARITAL_STATUS =
      new ProviderType("MARITAL_STATUS", "Marital Status", "Masks marital status");
  /** The constant RACE */
  public static final ProviderType RACE =
      new ProviderType("RACE", "Race/Ethnicity", "Masks races and ethnicities");
  /** The constant MAC_ADDRESS. */
  public static final ProviderType MAC_ADDRESS = new ProviderType("MAC_ADDRESS", "MAC Address",
      "Masks MAC addresses, supports vendor preservation");
  /** The constant MAINTAIN. */
  public static final ProviderType MAINTAIN =
      new ProviderType("MAINTAIN", "Maintain", "To maintain original value");
  /** The constant CREDIT_CARD_TYPE. */
  public static final ProviderType CREDIT_CARD_TYPE =
      new ProviderType("CREDIT_CARD_TYPE", "Credit Card type", "Mask credit card vendor names");
  /** The constant IBAN. */
  public static final ProviderType IBAN = new ProviderType("IBAN", "IBAN", "Masks IBAN values");
  /** The constant IMEI. */
  public static final ProviderType IMEI =
      new ProviderType("IMEI", "IMEI", "Masks IMEI values, supports vendor preservation");
  /** The constant SSN_UK. */
  public static final ProviderType SSN_UK =
      new ProviderType("SSN_UK", "Social Security Number UK", "Masks social security numbers");
  /** The constant SSN_US. */
  public static final ProviderType SSN_US =
      new ProviderType("SSN_US", "Social Security Number US", "Masks social security numbers");
  /** The constant OCCUPATION. */
  public static final ProviderType OCCUPATION =
      new ProviderType("OCCUPATION", "Occupation", "Masks occupations");
  /** The constant SWIFT. */
  public static final ProviderType SWIFT =
      new ProviderType("SWIFT", "SWIFT code", "Masks SWIFT codes, support country preservation");
  /** The constant GUID. */
  public static final ProviderType GUID =
      new ProviderType("GUID", "GUID", "Replaces values with a GUID");
  /** The constant EMPTY. */
  public static final ProviderType EMPTY =
      new ProviderType("EMPTY", "Empty", "Empty", TypeClass.CATEGORICAL, true);
  /** The constant ATC. */
  public static final ProviderType ATC = new ProviderType("ATC", "ATC codes", "Masks ATC codes");
  /** The constant MEDICINE. */
  public static final ProviderType MEDICINE =
      new ProviderType("MEDICINE", "Drug list", "Masks name of drugs");
  /** The constant REPLACE. */
  public static final ProviderType REPLACE = new ProviderType("REPLACE", "Replace",
      "Replaces and preserves parts of the value", TypeClass.CATEGORICAL, false);

  /** The constant NULL. */
  public static final ProviderType NULL = new ProviderType("NULL", "Null",
      "Replaces the value with an empty one", TypeClass.CATEGORICAL, false);

  public static final ProviderType HASH =
      new ProviderType("HASH", "Hash", "Hashes the value", TypeClass.CATEGORICAL, false);

  public static final ProviderType SHIFT =
      new ProviderType("SHIFT", "Shift", "Shifts the value", TypeClass.NUMERICAL, false);

  public static final ProviderType PSEUDONYM =
      new ProviderType("PSEUDONYM", "Pseudonym", "Replaces the value with a pseudonym");

  public static final ProviderType GENERALIZE = new ProviderType("GENERALIZE", "Generalize",
      "Replaces the value based on generalize ruleset");

  public static final ProviderType CONDITIONAL = new ProviderType("CONDITIONAL", "Conditional",
      "Replaces the value based on conditional ruleset");

  /** The constant STATES_US */
  public static final ProviderType STATES_US = new ProviderType("STATES_US", "US States",
      "Replaces the US States value", TypeClass.CATEGORICAL, false);

  public static final ProviderType GENDER =
      new ProviderType("GENDER", "Genders", "Replaces the genders");

  public static final ProviderType COUNTY =
      new ProviderType("COUNTY", "Counties", "Replaces the county names");

  public static final ProviderType FHIR =
      new ProviderType("FHIR", "FHIR objects", "Masks FHIR objects");

  public static final ProviderType HASHINT = new ProviderType("HASHINT", "Hash and convert to int",
      "Hashes the incoming (integer) value and returns an integer (as a string)");

  public static final ProviderType ZIPCODE =
      new ProviderType("ZIPCODE", "ZIP codes", "Replaces the ZIP codes");

  public static final ProviderType MRN =
      new ProviderType("MRN", "Medical Record Number", "Medical record number");

  public static final ProviderType ORGANIZATION =
      new ProviderType("ORGANIZATION", "Organizations", "Masks organizations");


  public static final ProviderType GENERIC = new ProviderType("GENERIC", "Generic Json Handler",
      "Mask generic json, not conforming to the FHIR standard");

  private final String unfriendlyName;
  private final String description;
  private final String friendlyName;
  private final int id;
  private final boolean forInternalPurposes;
  private final TypeClass typeClass;

  /**
   * Gets id.
   *
   * @return the id
   */
  @JsonIgnore
  public int getId() {
    return this.id;
  }

  /**
   * Gets name.
   *
   * @return the name
   */
  public String getName() {
    return this.unfriendlyName;
  }

  /**
   * Gets friendly name.
   *
   * @return the friendly name
   */
  public String getFriendlyName() {
    return this.friendlyName;
  }

  private boolean isForInternalPurposes() {
    return forInternalPurposes;
  }

  /**
   * Gets description.
   *
   * @return the description
   */
  public String getDescription() {
    return this.description;
  }

  /**
   * Gets type class.
   *
   * @return the type class
   */
  public TypeClass getTypeClass() {
    return typeClass;
  }

  private ProviderType(String unfriendlyName, String friendlyName, String description,
      TypeClass typeClass, boolean forInternalPurposes) {
    this.unfriendlyName = unfriendlyName;
    this.friendlyName = friendlyName;
    this.description = description;
    this.forInternalPurposes = forInternalPurposes;
    this.typeClass = typeClass;

    this.id = insertType(this);
  }

  private ProviderType(String name, String friendlyName, String description, TypeClass typeClass) {
    this(name, friendlyName, description, typeClass, false);
  }

  private ProviderType(String name, String friendlyName, String description) {
    this(name, friendlyName, description, TypeClass.CATEGORICAL, false);
  }

  private ProviderType(String name) {
    this(name, name, "");
  }

  @Override
  public boolean equals(Object o) {
    return !(null == o || !(o instanceof ProviderType))
        && this.unfriendlyName.equals(((ProviderType) o).getName());
  }

  @Override
  public String toString() {
    return getName();
  }

  /**
   * Value of provider type.
   *
   * @param name the name
   * @return the provider type
   */
  public static synchronized ProviderType valueOf(String name) {
    if (!registeredTypes.containsKey(name)) {
      return new ProviderType(name);
    }

    return registeredTypes.get(name);
  }

  /**
   * Public values collection.
   *
   * @return the collection
   */
  public static synchronized Collection<ProviderType> publicValues() {
    Collection<ProviderType> providerTypes = new ArrayList<>();

    for (ProviderType p : registeredTypes.values()) {
      if (!p.isForInternalPurposes()) {
        providerTypes.add(p);
      }
    }

    return providerTypes;
  }

  /**
   * Values provider type [ ].
   *
   * @return the provider type [ ]
   */
  public static synchronized ProviderType[] values() {
    ProviderType[] values = new ProviderType[registeredTypes.size()];
    return registeredTypes.values().toArray(values);
  }

  /**
   * Register type provider type.
   *
   * @param typeName the type name
   * @return the provider type
   */
  public static ProviderType registerType(String typeName) {
    ProviderType providerType = valueOf(typeName);
    if (providerType != null) {
      return providerType;
    }

    return new ProviderType(typeName);
  }

  private static synchronized int insertType(ProviderType type) {
    if (!registeredTypes.containsKey(type.getName())) {
      registeredTypes.put(type.getName(), type);
      return registeredTypes.size();
    }

    return registeredTypes.get(type.getName()).getId();
  }

  @Override
  public int hashCode() {
    return Objects.hash(unfriendlyName, description, friendlyName, id, forInternalPurposes,
        typeClass);
  }
}
