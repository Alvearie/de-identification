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
 * Masks MAC addresses with the option to preserve the vendor information.
 */
@JsonInclude(Include.NON_NULL)
public class MACAddressMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 856743324574648158L;
  private boolean maskingPreserveVendor = true;

  public MACAddressMaskingProviderConfig() {
    type = MaskingProviderType.MAC_ADDRESS;
  }

  public boolean isMaskingPreserveVendor() {
    return maskingPreserveVendor;
  }

  public void setMaskingPreserveVendor(boolean maskingPreserveVendor) {
    this.maskingPreserveVendor = maskingPreserveVendor;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (maskingPreserveVendor ? 1231 : 1237);
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
    MACAddressMaskingProviderConfig other = (MACAddressMaskingProviderConfig) obj;
    if (maskingPreserveVendor != other.maskingPreserveVendor)
      return false;
    return true;
  }
}
