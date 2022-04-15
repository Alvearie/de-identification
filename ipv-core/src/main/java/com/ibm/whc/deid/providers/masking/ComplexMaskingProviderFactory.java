/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.providers.masking.fhir.FHIRMaskingProvider;
import com.ibm.whc.deid.providers.masking.fhir.GenericMaskingProvider;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.GlobalProcessorConfig;

/**
 * Factory to return the masking driver for the format of data represented by the given schema type.
 */
public class ComplexMaskingProviderFactory {

  /**
   * Obtains a masking driver appropriate for data in a format that follows the given schema.
   * 
   * @param configSchemaType the schema that controls the format of the data
   * @param deidMaskingConfig the masking configuration used to create rule-level masking providers
   * @param maskingProviderFactory the factory used to create rule-level masking providers
   * @param tenantId the tenant making this request
   * 
   * @return the appropriate masking driver or <i>null</i> if not such driver can be retrieved
   */
  public ComplexMaskingProvider get(ConfigSchemaTypes configSchemaType,
      DeidMaskingConfig deidMaskingConfig, BasicMaskingProviderFactory maskingProviderFactory,
      String tenantId) {
    return get(configSchemaType, deidMaskingConfig, maskingProviderFactory, null, tenantId);
  }

  /**
   * Obtains a masking driver appropriate for data in a format that follows the given schema.
   * 
   * @param configSchemaType the schema that controls the format of the data
   * @param deidMaskingConfig the masking configuration used to create rule-level masking providers
   * @param maskingProviderFactory the factory used to create rule-level masking providers
   * @param globalProcessorConfig the configuration of the global processor, which may be
   *        <i>null</i> if Global Processor service is not requested
   * @param tenantId the tenant making this request
   * 
   * @return the appropriate masking driver or <i>null</i> if not such driver can be retrieved
   */
  public ComplexMaskingProvider get(ConfigSchemaTypes configSchemaType,
      DeidMaskingConfig deidMaskingConfig, BasicMaskingProviderFactory maskingProviderFactory,
      GlobalProcessorConfig globalProcessorConfig, String tenantId) {
    switch ((ConfigSchemaType) configSchemaType) {
      case FHIR:
        if (deidMaskingConfig.getJson() == null
            || deidMaskingConfig.getJson().getSchemaType() == null) {
          // return null if the masking config did not specify json schema
          return null;
        }
        if (globalProcessorConfig == null) {
          return new FHIRMaskingProvider(deidMaskingConfig, maskingProviderFactory, tenantId);
        }
        return new FHIRMaskingProvider(deidMaskingConfig, maskingProviderFactory,
            globalProcessorConfig, tenantId);
      case GEN:
        if (deidMaskingConfig.getJson() == null
            || deidMaskingConfig.getJson().getSchemaType() == null) {
          // return null if the masking config did not specify json schema
          return null;
        }
        if (globalProcessorConfig == null) {
          return new GenericMaskingProvider(deidMaskingConfig, maskingProviderFactory, tenantId);
        }
        return new GenericMaskingProvider(deidMaskingConfig, maskingProviderFactory,
            globalProcessorConfig, tenantId);
      default:
        throw new IllegalArgumentException("Unsupported type:" + configSchemaType);
    }
  }
}
