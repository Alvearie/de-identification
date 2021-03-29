/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.providers.masking.fhir.FHIRMaskingProvider;
import com.ibm.whc.deid.providers.masking.fhir.GenericMaskingProvider;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;

public class ComplexMaskingProviderFactory {
  /**
   * This is the entry point to parsing the masking configuration. Once the masking configuration is
   * obtained, it reads the schemaType and determines whether its FHIR/GEN/TEXT masking provider CSV
   * support to be added back soon
   */
  public synchronized MaskingProvider get(ConfigSchemaTypes configSchemaType,
      DeidMaskingConfig deidMaskingConfig, BasicMaskingProviderFactory maskingProviderFactory,
      String tenantId) {

    switch ((ConfigSchemaType) configSchemaType) {
      case FHIR:
        if (deidMaskingConfig.getJson().getSchemaType() == null) {
          // return null if the masking config did not specify json schema
          return null;
        }
        return new FHIRMaskingProvider(deidMaskingConfig, maskingProviderFactory, tenantId);
      case GEN:
        if (deidMaskingConfig.getJson().getSchemaType() == null) {
          // return null if the masking config did not specify json schema
          return null;
        }
        return new GenericMaskingProvider(deidMaskingConfig, maskingProviderFactory, tenantId);
      default:
        throw new IllegalArgumentException("Unsupported type:" + configSchemaType);
    }
  }
}
