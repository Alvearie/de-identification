/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/*
 * Masks an ATC code with the option to preserve certain levels.
 */
@JsonInclude(Include.NON_NULL)
public class ATCMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -1350698900871815535L;

  private int maskLevelsToKeep = 4;

  public ATCMaskingProviderConfig() {
    type = MaskingProviderType.ATC;
  }

  public int getMaskLevelsToKeep() {
    return maskLevelsToKeep;
  }

  public void setMaskLevelsToKeep(int maskLevelsToKeep) {
    this.maskLevelsToKeep = maskLevelsToKeep;
  }

  @Override
  public void validate(DeidMaskingConfig maskingConfig)
      throws InvalidMaskingConfigurationException {
    super.validate(maskingConfig);
    if (maskLevelsToKeep < 1 || maskLevelsToKeep > 4) {
      throw new InvalidMaskingConfigurationException("`maskLevelsToKeep` must be [1..4]");
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + maskLevelsToKeep;
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
    ATCMaskingProviderConfig other = (ATCMaskingProviderConfig) obj;
    if (maskLevelsToKeep != other.maskLevelsToKeep)
      return false;
    return true;
  }
}
