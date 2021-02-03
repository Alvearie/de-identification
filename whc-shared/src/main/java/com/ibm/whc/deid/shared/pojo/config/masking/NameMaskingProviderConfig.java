/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;

/*
 * Masks the first and last names of individuals.
 */
@JsonInclude(Include.NON_NULL)
public class NameMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -1111084929864098883L;
  private boolean maskingAllowUnisex = false;
  private boolean maskPseudorandom = false;
  private boolean maskGenderPreserve = true;

  public NameMaskingProviderConfig() {
    type = MaskingProviderType.NAME;
  }

  public boolean isMaskingAllowUnisex() {
    return maskingAllowUnisex;
  }

  public void setMaskingAllowUnisex(boolean maskingAllowUnisex) {
    this.maskingAllowUnisex = maskingAllowUnisex;
  }

  public boolean isMaskPseudorandom() {
    return maskPseudorandom;
  }

  public void setMaskPseudorandom(boolean maskPseudorandom) {
    this.maskPseudorandom = maskPseudorandom;
  }

  public boolean isMaskGenderPreserve() {
    return maskGenderPreserve;
  }

  public void setMaskGenderPreserve(boolean maskGenderPreserve) {
    this.maskGenderPreserve = maskGenderPreserve;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (maskGenderPreserve ? 1231 : 1237);
    result = prime * result + (maskPseudorandom ? 1231 : 1237);
    result = prime * result + (maskingAllowUnisex ? 1231 : 1237);
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
    NameMaskingProviderConfig other = (NameMaskingProviderConfig) obj;
    if (maskGenderPreserve != other.maskGenderPreserve)
      return false;
    if (maskPseudorandom != other.maskPseudorandom)
      return false;
    if (maskingAllowUnisex != other.maskingAllowUnisex)
      return false;
    return true;
  }
}
