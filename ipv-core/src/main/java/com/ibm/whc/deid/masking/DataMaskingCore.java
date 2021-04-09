/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.masking;

import java.io.IOException;
import java.util.List;
import com.ibm.whc.deid.providers.masking.BasicMaskingProviderFactory;
import com.ibm.whc.deid.providers.masking.ComplexMaskingProviderFactoryUtil;
import com.ibm.whc.deid.providers.masking.MaskingProvider;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.ReferableData;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import com.ibm.whc.deid.shared.util.MaskingConfigUtils;

public class DataMaskingCore {

  /**
   * @param input
   * @param maskingProvider
   * @param maskingOptions
   * @return
   */
  protected List<ReferableData> protectRecord(List<ReferableData> input,
      final MaskingProvider maskingProvider) {
    return maskingProvider.maskWithBatch(input, "REST");
  }

  /**
   * Given a configuration file, mask fields in a JSON string
   *
   * @param configuration
   * @param inputData
   * @param schemaType
   * @return
   * @throws IOException
   * @throws DeidException
   */
  public List<ReferableData> maskData(final String configuration,
      final List<ReferableData> inputData, ConfigSchemaTypes schemaType)
      throws IOException, DeidException {

    DeidMaskingConfig deidMaskingConfig;
    try {
      deidMaskingConfig = MaskingConfigUtils.validateConfig(configuration);
    } catch (InvalidMaskingConfigurationException e) {
      throw new DeidException(e.getMessage(), e);
    }

    MaskingProvider complexMaskingProvider =
        ComplexMaskingProviderFactoryUtil.getComplexMaskingProviderFactory().get(schemaType,
            deidMaskingConfig, new BasicMaskingProviderFactory(), null);

    return protectRecord(inputData, complexMaskingProvider);
  }
}
