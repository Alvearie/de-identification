/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition;

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
}
