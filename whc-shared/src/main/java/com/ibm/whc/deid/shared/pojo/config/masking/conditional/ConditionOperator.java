/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking.conditional;

import com.fasterxml.jackson.annotation.JsonValue;

public enum ConditionOperator {
  EQUALS("equals"), EQUALS_IGNORE_CASE("equalsIgnoreCase"), CONTAINS("contains"), CONTAINED_IN(
      "containedIn");

  private final String value;

  private ConditionOperator(String value) {
    this.value = value;
  }

  @JsonValue
  public String getValue() {
    return value;
  }
}
