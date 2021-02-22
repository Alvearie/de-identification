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
   * @param providerType the type of provider to create
 * @param deidMaskingConfig the complete masking provider configuration
 * @param config the configuration for the provider to create
 * @param tenantId the current tenant
 * @param localizationProperty TODO
   * @return the requested masking provider object
   * 
   * @throws NullPointerException if providerType or config are <i>null</i>
   */
  public MaskingProvider getProviderFromType(MaskingProviderTypes providerType,
      DeidMaskingConfig deidMaskingConfig, MaskingProviderConfig config, String tenantId, String localizationProperty);

}
