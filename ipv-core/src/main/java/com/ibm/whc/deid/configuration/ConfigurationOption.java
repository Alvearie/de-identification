/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.configuration;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.io.Serializable;

public class ConfigurationOption implements Serializable {
  /** */
  private static final long serialVersionUID = 3840287161612296131L;

  private final String description;
  private Object value;

  /**
   * Instantiates a new Configuration option.
   *
   * @param value the value
   * @param description the description
   * @param category the category
   */
  @JsonCreator
  public ConfigurationOption(@JsonProperty("value") Object value,
      @JsonProperty("description") String description) {
    this.description = description;
    this.value = value;
  }

  /**
   * Gets value.
   *
   * @return the value
   */
  public Object getValue() {
    return this.value;
  }

  /**
   * Sets value.
   *
   * @param value the value
   */
  public void setValue(Object value) {
    this.value = value;
  }

  /**
   * Gets description.
   *
   * @return the description
   */
  public String getDescription() {
    return this.description;
  }
}
