/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking.conditional;

import java.util.List;
import java.util.Objects;
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
  private List<String> valueList;

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

  public List<String> getValueList() {
    return valueList;
  }

  public void setValueList(List<String> valueList) {
    this.valueList = valueList;
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
    if (operator.usesList()) {
      if (valueList == null || valueList.isEmpty()) {
        throw new InvalidMaskingConfigurationException(
            "`" + String.valueOf(parentName) + ".valueList` is missing");
      }
      int offset = 0;
      for (String v : valueList) {
        if (v == null) {
          throw new InvalidMaskingConfigurationException(
              "`" + String.valueOf(parentName) + ".valueList[" + offset + "]` is missing");
        }
        offset++;
      }
    }

    /**
     * WHD-1930 commenting below code for supporting null value in CONDITIONAL
     */
    // else {
    // if (value == null) {
    // throw new InvalidMaskingConfigurationException(
    // "`" + String.valueOf(parentName) + ".value` is missing");
    // }
    // }
  }

  // generated code
  @Override
  public int hashCode() {
    return Objects.hash(field, operator, type, value, valueList);
  }

  // generated code
  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (!(obj instanceof Condition)) {
      return false;
    }
    Condition other = (Condition) obj;
    return Objects.equals(field, other.field) && operator == other.operator && type == other.type
        && Objects.equals(value, other.value) && Objects.equals(valueList, other.valueList);
  }
}
