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
 * Masks an occupation with a randomly selected occupation
 */
@JsonInclude(Include.NON_NULL)
public class OccupationMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 4922864365288665229L;
  boolean maskGeneralize = false;

  public OccupationMaskingProviderConfig() {
    type = MaskingProviderType.OCCUPATION;
  }

  public boolean isMaskGeneralize() {
    return maskGeneralize;
  }

  public void setMaskGeneralize(boolean maskGeneralize) {
    this.maskGeneralize = maskGeneralize;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (maskGeneralize ? 1231 : 1237);
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
    OccupationMaskingProviderConfig other = (OccupationMaskingProviderConfig) obj;
    if (maskGeneralize != other.maskGeneralize)
      return false;
    return true;
  }
}
