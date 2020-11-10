/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;

public interface MaskingProviderFactory {

  /**
   * Get a masking provider based on type.
   * 
   * @param providerType
   * @param deidMaskingConfig
   * @param config
   * @param tenantId
   * @return
   */
  public MaskingProvider getProviderFromType(MaskingProviderTypes providerType,
      DeidMaskingConfig deidMaskingConfig, MaskingProviderConfig config, String tenantId);

}
