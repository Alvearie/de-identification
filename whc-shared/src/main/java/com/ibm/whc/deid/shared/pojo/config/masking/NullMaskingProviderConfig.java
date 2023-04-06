/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;

/*
 * Replaces the original data value with an empty string (“”), or with a NULL value.
 */
@JsonInclude(Include.NON_NULL)
public class NullMaskingProviderConfig extends MaskingProviderConfig {
  private static final long serialVersionUID = -4334059186680999571L;
  private boolean maskReturnNull = true;

  public NullMaskingProviderConfig() {
    type = MaskingProviderType.NULL;
  }

  public boolean isMaskReturnNull() {
    return maskReturnNull;
  }

  public void setMaskReturnNull(boolean maskReturnNull) {
    this.maskReturnNull = maskReturnNull;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (maskReturnNull ? 1231 : 1237);
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
    NullMaskingProviderConfig other = (NullMaskingProviderConfig) obj;
    if (maskReturnNull != other.maskReturnNull)
      return false;
    return true;
  }
}
