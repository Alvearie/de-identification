/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking.conditional;

import com.fasterxml.jackson.annotation.JsonValue;

public enum ConditionOperator {
  //@formatter:off
  EQUALS("equals", false), 
  EQUALS_IGNORE_CASE("equalsIgnoreCase", false), 
  CONTAINS("contains", false), 
  CONTAINED_IN("containedIn", false),
  ANY_OF("anyOf", true),
  NOT_ANY_OF("notAnyOf", true),
  ANY_OF_IGNORE_CASE("anyOfIgnoreCase", true),
  NOT_ANY_OF_IGNORE_CASE("notAnyOfIgnoreCase", true)  
  ;
  //@formatter:on

  private final String value;
  private final boolean usesListOfValues;

  private ConditionOperator(String value, boolean usesList) {
    this.value = value;
    this.usesListOfValues = usesList;
  }

  @JsonValue
  public String getValue() {
    return value;
  }

  public boolean usesList() {
    return usesListOfValues;
  }
}
