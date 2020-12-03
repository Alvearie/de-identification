/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/*
 * Provider for masking a data element based on the value of another data element
 */
@JsonInclude(Include.NON_NULL)
public class ConditionalMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -5097975333785542411L;

  private List<ConditionalMaskRuleSet> maskRuleSet;

  public ConditionalMaskingProviderConfig() {
    type = MaskingProviderType.CONDITIONAL;
  }

  public List<ConditionalMaskRuleSet> getMaskRuleSet() {
    return maskRuleSet;
  }

  public void setMaskRuleSet(List<ConditionalMaskRuleSet> maskRuleSet) {
    this.maskRuleSet = maskRuleSet;
  }

  @Override
  public void validate() throws InvalidMaskingConfigurationException {
    if (maskRuleSet == null || maskRuleSet.isEmpty()) {
      throw new InvalidMaskingConfigurationException(
          "`maskRuleSet` must be specified with at least one entry");
    }
    for (ConditionalMaskRuleSet ruleSet : maskRuleSet) {
      if (ruleSet == null) {
        throw new InvalidMaskingConfigurationException("`maskRuleSet` is null");
      }
      ruleSet.validate("maskRuleSet");
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((maskRuleSet == null) ? 0 : maskRuleSet.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (!super.equals(obj))
      return false;
    if (getClass() != obj.getClass())
      return false;
    ConditionalMaskingProviderConfig other = (ConditionalMaskingProviderConfig) obj;
    if (maskRuleSet == null) {
      if (other.maskRuleSet != null)
        return false;
    } else if (!maskRuleSet.equals(other.maskRuleSet))
      return false;
    return true;
  }
}
