/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/*
 * Masks a continent by replacing it with a randomly selected, or with the closest continent.
 */
@JsonInclude(Include.NON_NULL)
public class ContinentMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 1341491397370614455L;

  private boolean maskClosest = false;
  private int maskClosestK = 5;

  public ContinentMaskingProviderConfig() {
    type = MaskingProviderType.CONTINENT;
  }

  public boolean isMaskClosest() {
    return maskClosest;
  }

  public void setMaskClosest(boolean maskClosest) {
    this.maskClosest = maskClosest;
  }

  public int getMaskClosestK() {
    return maskClosestK;
  }

  public void setMaskClosestK(int maskClosestK) {
    this.maskClosestK = maskClosestK;
  }

  @Override
  public void validate() throws InvalidMaskingConfigurationException {
    super.validate();
    if (maskClosestK < 1) {
      throw new InvalidMaskingConfigurationException("`maskClosestK` must be greater than 0");
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (maskClosest ? 1231 : 1237);
    result = prime * result + maskClosestK;
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
    ContinentMaskingProviderConfig other = (ContinentMaskingProviderConfig) obj;
    if (maskClosest != other.maskClosest)
      return false;
    if (maskClosestK != other.maskClosestK)
      return false;
    return true;
  }
}
