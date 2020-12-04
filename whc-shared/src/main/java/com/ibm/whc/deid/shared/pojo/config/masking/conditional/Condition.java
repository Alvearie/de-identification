/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking.conditional;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/*
 * This class is used during the ConditionalMaskingProvider
 */
@JsonInclude(Include.NON_NULL)
public class Condition {

  private String field;
  private ConditionOperator operator;
  private ConditionType type;
  private String value;

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

  public void validate(String parentName) throws InvalidMaskingConfigurationException {
    if (field == null || field.trim().isEmpty()) {
      throw new InvalidMaskingConfigurationException(
          "`" + String.valueOf(parentName) + ".field` is missing");
    }
    if (operator == null) {
      throw new InvalidMaskingConfigurationException(
          "`" + String.valueOf(parentName) + ".operator` is missing");
    }
    // conditionType is not currently used
    if (value == null) {
      throw new InvalidMaskingConfigurationException(
          "`" + String.valueOf(parentName) + ".value` is missing");
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = prime + (field == null ? 0 : field.hashCode());
    result = prime * result + (operator == null ? 0 : operator.hashCode());
    result = prime * result + (type == null ? 0 : type.hashCode());
    result = prime * result + (value == null ? 0 : value.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    Condition other = (Condition) obj;
    return (field == null ? other.field == null : field.equals(other.field))
        && (operator == null ? other.operator == null : operator.equals(other.operator))
        && (type == null ? other.type == null : type.equals(other.type))
        && (value == null ? other.value == null : value.equals(other.value));
  }

}
