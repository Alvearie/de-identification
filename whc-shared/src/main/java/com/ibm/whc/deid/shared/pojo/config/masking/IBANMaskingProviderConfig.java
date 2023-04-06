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
 * Masks IBAN account numbers with the option to preserve country.
 */
@JsonInclude(Include.NON_NULL)
public class IBANMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -6151172927082419323L;

  boolean maskPreserveCountry = true;

  public IBANMaskingProviderConfig() {
    type = MaskingProviderType.IBAN;
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
    IBANMaskingProviderConfig other = (IBANMaskingProviderConfig) obj;
    if (maskPreserveCountry != other.maskPreserveCountry)
      return false;
    return true;
  }
}
