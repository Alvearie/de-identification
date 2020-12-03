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
 * Masks Social Security Numbers based on the US format
 */
@JsonInclude(Include.NON_NULL)
public class SSNUSMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -4420514859545185702L;
  private boolean maskPreserveAreaNumber = true;
  private boolean maskPreserveGroup = true;

  public SSNUSMaskingProviderConfig() {
    type = MaskingProviderType.SSN_US;
  }

  public boolean isMaskPreserveAreaNumber() {
    return maskPreserveAreaNumber;
  }

  public void setMaskPreserveAreaNumber(boolean maskPreserveAreaNumber) {
    this.maskPreserveAreaNumber = maskPreserveAreaNumber;
  }

  public boolean isMaskPreserveGroup() {
    return maskPreserveGroup;
  }

  public void setMaskPreserveGroup(boolean maskPreserveGroup) {
    this.maskPreserveGroup = maskPreserveGroup;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (maskPreserveAreaNumber ? 1231 : 1237);
    result = prime * result + (maskPreserveGroup ? 1231 : 1237);
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
    SSNUSMaskingProviderConfig other = (SSNUSMaskingProviderConfig) obj;
    if (maskPreserveAreaNumber != other.maskPreserveAreaNumber)
      return false;
    if (maskPreserveGroup != other.maskPreserveGroup)
      return false;
    return true;
  }
}
