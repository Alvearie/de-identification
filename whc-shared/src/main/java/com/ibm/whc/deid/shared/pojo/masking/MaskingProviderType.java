/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

/*
 * An enumeration of available masking providers
 */
public enum MaskingProviderType implements MaskingProviderTypes {

  // @formatter:off
  ADDRESS(Constants.ADDRESS_VALUE, MaskingProviderCategory.CategoryI),
  ATC(Constants.ATC_VALUE, MaskingProviderCategory.CategoryI),
  BINNING(Constants.BINNING_VALUE, MaskingProviderCategory.CategoryII),
  CITY(Constants.CITY_VALUE, MaskingProviderCategory.CategoryI),
  CONDITIONAL(Constants.CONDITIONAL_VALUE, MaskingProviderCategory.CategoryII),
  CONTINENT(Constants.CONTINENT_VALUE, MaskingProviderCategory.CategoryI),
  COUNTRY(Constants.COUNTRY_VALUE, MaskingProviderCategory.CategoryI),
  COUNTY(Constants.COUNTY_VALUE, MaskingProviderCategory.CategoryI),
  CREDIT_CARD(Constants.CREDIT_CARD_VALUE, MaskingProviderCategory.CategoryI),
  DATEDEPENDENCY(Constants.DATEDEPENDENCY_VALUE, MaskingProviderCategory.CategoryI),
  DATETIME(Constants.DATETIME_VALUE, MaskingProviderCategory.CategoryI),
  DATETIME_CONSISTENT_SHIFT(Constants.DATETIME_CONSISTENT_SHIFT_VALUE, MaskingProviderCategory.CategoryI),
  EMAIL(Constants.EMAIL_VALUE, MaskingProviderCategory.CategoryI),
  FPE(Constants.FPE_VALUE, MaskingProviderCategory.CategoryI),
  GENDER(Constants.GENDER_VALUE, MaskingProviderCategory.CategoryI),
  GEN(Constants.GEN_VALUE, MaskingProviderCategory.CategoryII),
  GENERALIZE(Constants.GENERALIZE_VALUE, MaskingProviderCategory.CategoryII),
  GUID(Constants.GUID_VALUE, MaskingProviderCategory.CategoryII),
  FHIR(Constants.FHIR_VALUE, MaskingProviderCategory.CategoryII),
  HASH(Constants.HASH_VALUE, MaskingProviderCategory.CategoryII),
  HOSPITAL(Constants.HOSPITAL_VALUE, MaskingProviderCategory.CategoryI),
  IBAN(Constants.IBAN_VALUE, MaskingProviderCategory.CategoryI),
  ICDV9(Constants.ICDV9_VALUE, MaskingProviderCategory.CategoryI),
  ICDV10(Constants.ICDV10_VALUE, MaskingProviderCategory.CategoryI),
  IMEI(Constants.IMEI_VALUE, MaskingProviderCategory.CategoryI),
  IP_ADDRESS(Constants.IP_ADDRESS_VALUE, MaskingProviderCategory.CategoryI),
  LATITUDE_LONGITUDE(Constants.LATITUDE_LONGITUDE_VALUE, MaskingProviderCategory.CategoryI),
  MARITAL(Constants.MARITAL_VALUE, MaskingProviderCategory.CategoryI),
  MAC_ADDRESS(Constants.MAC_ADDRESS_VALUE, MaskingProviderCategory.CategoryI),
  MAINTAIN(Constants.MAINTAIN_VALUE, MaskingProviderCategory.CategoryII),
  NAME(Constants.NAME_VALUE, MaskingProviderCategory.CategoryI),
  NULL(Constants.NULL_VALUE, MaskingProviderCategory.CategoryII),
  NUMBERVARIANCE(Constants.NUMBERVARIANCE_VALUE, MaskingProviderCategory.CategoryII),
  OCCUPATION(Constants.OCCUPATION_VALUE, MaskingProviderCategory.CategoryI),
  PHONE(Constants.PHONE_VALUE, MaskingProviderCategory.CategoryI),
  PSEUDONYM(Constants.PSEUDONYM_VALUE, MaskingProviderCategory.CategoryII),
  RACE(Constants.RACE_VALUE, MaskingProviderCategory.CategoryI),
  RANDOM(Constants.RANDOM_VALUE, MaskingProviderCategory.CategoryII),
  REDACT(Constants.REDACT_VALUE, MaskingProviderCategory.CategoryII),
  RELIGION(Constants.RELIGION_VALUE, MaskingProviderCategory.CategoryI),
  REPLACE(Constants.REPLACE_VALUE, MaskingProviderCategory.CategoryII),
  SSN_UK(Constants.SSN_UK_VALUE, MaskingProviderCategory.CategoryI),
  SSN_US(Constants.SSN_US_VALUE, MaskingProviderCategory.CategoryI),
  STATE_US(Constants.STATE_US_VALUE, MaskingProviderCategory.CategoryI),
  SWIFT(Constants.SWIFT_VALUE, MaskingProviderCategory.CategoryI),
  URL(Constants.URL_VALUE, MaskingProviderCategory.CategoryI),
  VIN(Constants.VIN_VALUE, MaskingProviderCategory.CategoryI),
  ZIPCODE(Constants.ZIPCODE_VALUE, MaskingProviderCategory.CategoryI);
  // @formatter:on

  private final String identifier;
  private final MaskingProviderCategory category;

  private MaskingProviderType(String identifier, MaskingProviderCategory category) {
    this.identifier = identifier;
    this.category = category;
  }

  @Override
  public String getIdentifier() {
    return identifier;
  }

  @Override
  public MaskingProviderCategory getCategory() {
    return category;
  }

  public static enum MaskingProviderCategory {
    /**
     * Providers that work with a specific type of protected information.
     */
    CategoryI("CategoryI"),
    
    /**
     * Providers that work with many types of information.
     */
    CategoryII("CategoryII");
    
    private final String displayName;

    private MaskingProviderCategory(String dn) {
      displayName = dn;
    }

    public String getMetadataDisplayName() {
      return displayName;
    }
  }

  public static class Constants {
    // In addition to enums, we need to define constant values for
    // annotations in {@link MaskingProviderConfig}
    public static final String ADDRESS_VALUE = "ADDRESS";
    public static final String ATC_VALUE = "ATC";
    public static final String BINNING_VALUE = "BINNING";
    public static final String CITY_VALUE = "CITY";
    public static final String COUNTY_VALUE = "COUNTY";
    public static final String CONDITIONAL_VALUE = "CONDITIONAL";
    public static final String COUNTRY_VALUE = "COUNTRY";
    public static final String CONTINENT_VALUE = "CONTINENT";
    public static final String CREDIT_CARD_VALUE = "CREDIT_CARD";
    public static final String DATETIME_VALUE = "DATETIME";
    public static final String DATETIME_CONSISTENT_SHIFT_VALUE = "DATETIME_CONSISTENT_SHIFT";
    public static final String DATEDEPENDENCY_VALUE = "DATEDEPENDENCY";
    public static final String EMAIL_VALUE = "EMAIL";
    public static final String FHIR_VALUE = "FHIR";
    public static final String FPE_VALUE = "FPE";
    public static final String GENERALIZE_VALUE = "GENERALIZE";
    public static final String GENDER_VALUE = "GENDER";
    public static final String GEN_VALUE = "GEN";
    public static final String GUID_VALUE = "GUID";
    public static final String HASH_VALUE = "HASH";
    public static final String HOSPITAL_VALUE = "HOSPITAL";
    public static final String IBAN_VALUE = "IBAN";
    public static final String ICDV9_VALUE = "ICDV9";
    public static final String ICDV10_VALUE = "ICDV10";
    public static final String IMEI_VALUE = "IMEI";
    public static final String IP_ADDRESS_VALUE = "IP_ADDRESS";
    public static final String LATITUDE_LONGITUDE_VALUE = "LATITUDE_LONGITUDE";
    public static final String MAC_ADDRESS_VALUE = "MAC_ADDRESS";
    public static final String MAINTAIN_VALUE = "MAINTAIN";
    public static final String MARITAL_VALUE = "MARITAL";
    public static final String NAME_VALUE = "NAME";
    public static final String NULL_VALUE = "NULL";
    public static final String NUMBERVARIANCE_VALUE = "NUMBERVARIANCE";
    public static final String OCCUPATION_VALUE = "OCCUPATION";
    public static final String PHONE_VALUE = "PHONE";
    public static final String PSEUDONYM_VALUE = "PSEUDONYM";
    public static final String RACE_VALUE = "RACE";
    public static final String RANDOM_VALUE = "RANDOM";
    public static final String REDACT_VALUE = "REDACT";
    public static final String RELIGION_VALUE = "RELIGION";
    public static final String REPLACE_VALUE = "REPLACE";
    public static final String SSN_UK_VALUE = "SSN_UK";
    public static final String SSN_US_VALUE = "SSN_US";
    public static final String STATE_US_VALUE = "STATE_US";
    public static final String SWIFT_VALUE = "SWIFT";
    public static final String URL_VALUE = "URL";
    public static final String VIN_VALUE = "VIN";
    public static final String ZIPCODE_VALUE = "ZIPCODE";
  }
}
