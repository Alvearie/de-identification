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
 * Masks a county by replacing it with a random county.
 */
@JsonInclude(Include.NON_NULL)
public class CountyMaskingProviderConfig extends MaskingProviderConfig {
  private static final long serialVersionUID = 3872346136437521705L;
  boolean maskPseudorandom = false;

  public CountyMaskingProviderConfig() {
    type = MaskingProviderType.CITY;
  }

  public boolean isMaskPseudorandom() {
    return maskPseudorandom;
  }

  public void setMaskPseudorandom(boolean maskPseudorandom) {
    this.maskPseudorandom = maskPseudorandom;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (maskPseudorandom ? 1231 : 1237);
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
    CountyMaskingProviderConfig other = (CountyMaskingProviderConfig) obj;
    if (maskPseudorandom != other.maskPseudorandom)
      return false;
    return true;
  }
}
