/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/*
 * RuleSet used in the ConditionalMaskingProviderConfig
 */
@JsonInclude(Include.NON_NULL)
public class ConditionalMaskRuleSet {

  private Condition condition;
  private MaskingProviderConfig maskingProvider;

  public Condition getCondition() {
    return condition;
  }

  public void setCondition(Condition condition) {
    this.condition = condition;
  }

  public MaskingProviderConfig getMaskingProvider() {
    return maskingProvider;
  }

  public void setMaskingProvider(MaskingProviderConfig maskingProvider) {
    this.maskingProvider = maskingProvider;
  }

  public void validate(String parentName) throws InvalidMaskingConfigurationException {
    if (condition != null) {
      condition.validate(String.valueOf(parentName) + ".condition");
    }
    if (maskingProvider == null) {
      throw new InvalidMaskingConfigurationException(
          "`" + String.valueOf(parentName) + ".maskingProvider` is missing");
    }
    try {
      maskingProvider.validate();
    } catch (InvalidMaskingConfigurationException e) {
      throw new InvalidMaskingConfigurationException(
          "`" + String.valueOf(parentName) + ".maskingProvider` is invalid: " + e.getMessage(), e);
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = prime + (condition == null ? 0 : condition.hashCode());
    result = prime * result + (maskingProvider == null ? 0 : maskingProvider.hashCode());
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
    ConditionalMaskRuleSet other = (ConditionalMaskRuleSet) obj;
    return (condition == null ? other.condition == null : condition.equals(other.condition))
        && (maskingProvider == null ? other.maskingProvider == null
            : maskingProvider.equals(other.maskingProvider));
  }
}
