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
 * Masks a city with a randomly selected city, or based on one of its neighboring cities
 * (geographical distance).
 */
@JsonInclude(Include.NON_NULL)
public class CityMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 4513266739679517581L;

  private boolean maskClosest;
  private int maskClosestK = 10;
  private boolean maskPseudorandom;

  public CityMaskingProviderConfig() {
    type = MaskingProviderType.CITY;
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

  public boolean isMaskPseudorandom() {
    return maskPseudorandom;
  }

  public void setMaskPseudorandom(boolean maskPseudorandom) {
    this.maskPseudorandom = maskPseudorandom;
  }

  @Override
  public void validate() throws InvalidMaskingConfigurationException {
    if (getMaskClosestK() < 1) {
      throw new InvalidMaskingConfigurationException("`maskClosestK` must be greater than 0");
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (maskClosest ? 1231 : 1237);
    result = prime * result + maskClosestK;
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
    CityMaskingProviderConfig other = (CityMaskingProviderConfig) obj;
    if (maskClosest != other.maskClosest)
      return false;
    if (maskClosestK != other.maskClosestK)
      return false;
    if (maskPseudorandom != other.maskPseudorandom)
      return false;
    return true;
  }
}
