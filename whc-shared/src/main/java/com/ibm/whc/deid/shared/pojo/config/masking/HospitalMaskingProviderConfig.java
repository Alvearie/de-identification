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
 * Replaces a hospital name with a randomly chosen one.
 */
@JsonInclude(Include.NON_NULL)
public class HospitalMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 7373588413833547589L;
  boolean maskPreserveCountry = true;

  public HospitalMaskingProviderConfig() {
    type = MaskingProviderType.HOSPITAL;
  }

  public boolean isMaskPreserveCountry() {
    return maskPreserveCountry;
  }

  public void setMaskPreserveCountry(boolean maskPreserveCountry) {
    this.maskPreserveCountry = maskPreserveCountry;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (maskPreserveCountry ? 1231 : 1237);
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
    HospitalMaskingProviderConfig other = (HospitalMaskingProviderConfig) obj;
    if (maskPreserveCountry != other.maskPreserveCountry)
      return false;
    return true;
  }
}
