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

  public void setPreserveTac(boolean preserveTAC) {
    this.preserveTAC = preserveTAC;
  }
}
