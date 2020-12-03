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
 * Masks a vehicle’s identifier number with options to preserve the manufacturer and the vehicle
 * description information.
 */
@JsonInclude(Include.NON_NULL)
public class VINMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -3617565031465171733L;
  private boolean wmiPreserve = true;
  private boolean vdsPreserve = false;

  public VINMaskingProviderConfig() {
    type = MaskingProviderType.VIN;
  }

  public boolean isWmiPreserve() {
    return wmiPreserve;
  }

  public void setWmiPreserve(boolean wmiPreserve) {
    this.wmiPreserve = wmiPreserve;
  }

  public boolean isVdsPreserve() {
    return vdsPreserve;
  }

  public void setVdsPreserve(boolean vdsPreserve) {
    this.vdsPreserve = vdsPreserve;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (vdsPreserve ? 1231 : 1237);
    result = prime * result + (wmiPreserve ? 1231 : 1237);
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
    VINMaskingProviderConfig other = (VINMaskingProviderConfig) obj;
    if (vdsPreserve != other.vdsPreserve)
      return false;
    if (wmiPreserve != other.wmiPreserve)
      return false;
    return true;
  }
}
