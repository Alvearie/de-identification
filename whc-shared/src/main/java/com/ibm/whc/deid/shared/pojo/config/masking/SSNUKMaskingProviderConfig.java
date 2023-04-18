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
 * Masks a social security number (SSN) based on the UK format
 */
@JsonInclude(Include.NON_NULL)
public class SSNUKMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 2452766437179367170L;
  private boolean maskPreservePrefix = true;

  public SSNUKMaskingProviderConfig() {
    type = MaskingProviderType.SSN_UK;
  }

  public boolean isMaskPreservePrefix() {
    return maskPreservePrefix;
  }

  public void setMaskPreservePrefix(boolean maskPreservePrefix) {
    this.maskPreservePrefix = maskPreservePrefix;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (maskPreservePrefix ? 1231 : 1237);
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
    SSNUKMaskingProviderConfig other = (SSNUKMaskingProviderConfig) obj;
    if (maskPreservePrefix != other.maskPreservePrefix)
      return false;
    return true;
  }
}
