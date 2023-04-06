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
 * Masks SWIFT codes with the option to preserve their country.
 */
@JsonInclude(Include.NON_NULL)
public class SWIFTMaskingProviderConfig extends MaskingProviderConfig {
  private static final long serialVersionUID = -1616246339064360568L;

  private boolean preserveCountry = false;

  public SWIFTMaskingProviderConfig() {
    type = MaskingProviderType.SWIFT;
  }

  public boolean isPreserveCountry() {
    return preserveCountry;
  }

  public void setPreserveCountry(boolean preserveCountry) {
    this.preserveCountry = preserveCountry;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (preserveCountry ? 1231 : 1237);
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
    SWIFTMaskingProviderConfig other = (SWIFTMaskingProviderConfig) obj;
    if (preserveCountry != other.preserveCountry)
      return false;
    return true;
  }
}
