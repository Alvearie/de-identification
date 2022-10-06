/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import com.ibm.whc.deid.providers.masking.MaskingProviderFactory;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;

public class GenericMaskingProvider extends FHIRMaskingProvider {

  private static final long serialVersionUID = 5945527984023679481L;

  private static final String BASE_PATH_PREFIX = "/gen/";

  public GenericMaskingProvider(DeidMaskingConfig maskingConfiguration,
      MaskingProviderFactory maskingProviderFactory, String tenantId) {
    super(maskingConfiguration, maskingConfiguration.isDefaultNoRuleResolution(),
        maskingProviderFactory, BASE_PATH_PREFIX, tenantId);
  }
}
