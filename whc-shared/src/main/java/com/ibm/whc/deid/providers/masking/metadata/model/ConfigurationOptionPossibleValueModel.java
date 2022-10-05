/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.metadata.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

/**
 * Container for a description of a supported value for a masking provider configuration option.
 * Used when only a finite set of values with specific meanings are supported.
 */

@JsonPropertyOrder({"value", "displayName"})
public class ConfigurationOptionPossibleValueModel {

  /**
   * a supported internal value for a configuration option
   */
  private String value;

  /**
   * a user-friendly name and description for the configuration option value
   */
  @JsonProperty("displayName")
  private String valueDisplayName;

  public ConfigurationOptionPossibleValueModel() {
    // initialize with all null members
  }

  public ConfigurationOptionPossibleValueModel(String value, String display) {
    this.value = value;
    this.valueDisplayName = display;
  }

  /**
   * @return the value
   */
  public String getValue() {
    return value;
  }

  /**
   * @param value the value to set
   */
  public void setValue(String value) {
    this.value = value;
  }

  /**
   * @return the valueDisplayName
   */
  public String getValueDisplayName() {
    return valueDisplayName;
  }

  /**
   * @param valueDisplayName the valueDisplayName to set
   */
  public void setValueDisplayName(String valueDisplayName) {
    this.valueDisplayName = valueDisplayName;
  }
}
