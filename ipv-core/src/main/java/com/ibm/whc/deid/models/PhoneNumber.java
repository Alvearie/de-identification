/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;

public class PhoneNumber implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = 8091553131228609026L;
	private String prefix;
  private String countryCode;
  private String areaCode;
  private String separator;
  private String number;

  /**
   * Is has prefix boolean.
   *
   * @return the boolean
   */
  public boolean isHasPrefix() {
    return hasPrefix;
  }

  private boolean hasPrefix;

  /**
   * Gets country code.
   *
   * @return the country code
   */
  public String getCountryCode() {
    return countryCode;
  }

  /**
   * Gets prefix.
   *
   * @return the prefix
   */
  public String getPrefix() {
    return prefix;
  }

  /**
   * Gets separator.
   *
   * @return the separator
   */
  public String getSeparator() {
    return separator;
  }

  /**
   * Gets number.
   *
   * @return the number
   */
  public String getNumber() {
    return number;
  }

  /**
   * Gets area code.
   *
   * @return the area code
   */
  public String getAreaCode() {
    return this.areaCode;
  }

  /**
   * Sets area code.
   *
   * @param areaCode the area code
   */
  public void setAreaCode(String areaCode) {
    this.areaCode = areaCode;
  }

  /**
   * Instantiates a new Phone number.
   *
   * @param prefix the prefix
   * @param countryCode the country code
   * @param separator the separator
   * @param number the number
   * @param hasPrefix the has prefix
   */
  public PhoneNumber(String prefix, String countryCode, String separator, String number,
      boolean hasPrefix) {
    this(prefix, countryCode, separator, number, "", hasPrefix);
  }

  /**
   * Instantiates a new Phone number.
   *
   * @param prefix the prefix
   * @param countryCode the country code
   * @param separator the separator
   * @param number the number
   * @param areaCode the area code
   * @param hasPrefix the has prefix
   */
  public PhoneNumber(String prefix, String countryCode, String separator, String number,
      String areaCode, boolean hasPrefix) {
    this.prefix = prefix;
    this.countryCode = countryCode;
    this.separator = separator;
    this.number = number;
    this.areaCode = areaCode;
    this.hasPrefix = hasPrefix;
  }
}
