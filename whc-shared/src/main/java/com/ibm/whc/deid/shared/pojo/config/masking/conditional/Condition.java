/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking.conditional;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

/*
 * This class is used during the ConditionalMaskingProvider
 */
@JsonInclude(Include.NON_NULL)
public class Condition {
  String field;
  ConditionOperator operator;
  ConditionType type;
  String value;

  public String getField() {
    return field;
  }

  public void setField(String field) {
    this.field = field;
  }

  public ConditionOperator getOperator() {
    return operator;
  }

  public void setOperator(ConditionOperator operator) {
    this.operator = operator;
  }

  public ConditionType getType() {
    return type;
  }

  public void setType(ConditionType type) {
    this.type = type;
  }

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    this.value = value;
  }
}
