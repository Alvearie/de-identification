/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.masking;

import java.util.List;
import com.ibm.whc.deid.providers.masking.BasicMaskingProviderFactory;
import com.ibm.whc.deid.providers.masking.ComplexMaskingProvider;
import com.ibm.whc.deid.providers.masking.ComplexMaskingProviderFactoryUtil;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.ReferableData;

public class DataMaskingCore {

  /**
   * Given a configuration file, mask fields in a JSON document.
   *
   * @param deidMaskingConfig masking configuration
   * @param schemaType the format of the documents to process
   * @param list JSON documents to process
   * 
   * @return the processed documents with their identifiers
   */
  public List<ReferableData> maskData(final DeidMaskingConfig deidMaskingConfig,
      final List<ReferableData> inputData, ConfigSchemaTypes schemaType) {
    ComplexMaskingProvider complexMaskingProvider =
        ComplexMaskingProviderFactoryUtil.getComplexMaskingProviderFactory().get(schemaType,
            deidMaskingConfig, new BasicMaskingProviderFactory(), null);
    return protectRecord(inputData, complexMaskingProvider);
  }

  /**
   * @param input
   * @param maskingProvider
   * @param maskingOptions
   * @return
   */
  protected List<ReferableData> protectRecord(List<ReferableData> input,
      final ComplexMaskingProvider maskingProvider) {
    return maskingProvider.maskWithBatch(input, "REST");
  }
}
