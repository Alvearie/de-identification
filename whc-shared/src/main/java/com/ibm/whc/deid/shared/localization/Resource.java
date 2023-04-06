/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.localization;

public enum Resource implements Resources {
  /** Public suffix list resource. */
  PUBLIC_SUFFIX_LIST,
  /** Swift resource. */
  SWIFT,
  /** Religion resource. */
  RELIGION,
  /** Marital status resource. */
  MARITAL_STATUS,
  /** Credit card type resource. */
  CREDIT_CARD_TYPE,
  /** Occupation resource. */
  OCCUPATION,
  /** City resource. */
  CITY,
  /** Continent resource. */
  CONTINENT,
  /** Country resource. */
  COUNTRY,
  /** Phone calling codes resource. */
  PHONE_CALLING_CODES,
  /** Phone area codes resource. */
  PHONE_AREA_CODES,
  /** Postal codes resource. */
  POSTAL_CODES,
  /** Wmi resource. */
  WORLD_MANUFACTURERS_IDENTIFIER,
  /** Street names resource. */
  STREET_NAMES,
  /** First name female resource. */
  FIRST_NAME_FEMALE,
  /** First name male resource. */
  FIRST_NAME_MALE,
  /** Last name resource. */
  LAST_NAME,
  /** Atc codes resource. */
  ATC_CODES,
  /** Ic dv 10 resource. */
  ICDV10,
  /** Ic dv 9 resource. */
  ICDV9,
  /** Hospital names resource. */
  HOSPITAL_NAMES,
  /** Tacdb resource. */
  TACDB,
  /** RACE_ETHNICITY resource */
  RACE_ETHNICITY,
  /** States us resource. */
  STATES_US, GENDER, COUNTY, ZIPCODE, GENERALIZE, PATTERN, PHONE_NUM_DIGITS,
  /** types of stree. eg. St./blvd/Ave */
  STREET_TYPES,
  /** UK Social Security Number prefix. eg AB/ZE/YR */
  SSNUK_PREFIXES;

  @Override
  public String toString() {
    return name().toLowerCase();
  }
}
