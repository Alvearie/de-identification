/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;

/*
 * Replaces one or more specified original values with a specified general category term to which
 * these values belong.
 */
@JsonInclude(Include.NON_NULL)
public class GeneralizeMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 1395588026091172621L;
  String maskRuleSet = null;

  public GeneralizeMaskingProviderConfig() {
    type = MaskingProviderType.GENERALIZE;
  }

  public String getMaskRuleSet() {
    return maskRuleSet;
  }

  public void setMaskRuleSet(String maskRuleSet) {
    this.maskRuleSet = maskRuleSet;
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
    GeneralizeMaskingProviderConfig other = (GeneralizeMaskingProviderConfig) obj;
    if (maskRuleSet == null) {
      if (other.maskRuleSet != null)
        return false;
    } else if (!maskRuleSet.equals(other.maskRuleSet))
      return false;
    return true;
  }
}
