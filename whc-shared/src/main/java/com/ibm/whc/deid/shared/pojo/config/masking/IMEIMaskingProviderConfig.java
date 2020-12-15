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
 * Masks IMEI device identifiers.
 */
@JsonInclude(Include.NON_NULL)
public class IMEIMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -9045992293711157386L;

  // "The first eight digits of an IMEI make up the Type Allocation Code (TAC). The TAC indicates
  // the manufacturer and model of the particular device.
  // So, all device models from a particular manufacturer will have the same TAC."
  private boolean preserveTAC = true;

  public IMEIMaskingProviderConfig() {
    type = MaskingProviderType.IMEI;
  }

  public boolean getPreserveTAC() {
    return preserveTAC;
  }

  public void setPreserveTAC(boolean preserveTAC) {
    this.preserveTAC = preserveTAC;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (preserveTAC ? 1231 : 1237);
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    return (this == obj) || (obj != null && super.equals(obj) && getClass() == obj.getClass()
        && preserveTAC == ((IMEIMaskingProviderConfig) obj).preserveTAC);
  }

}
