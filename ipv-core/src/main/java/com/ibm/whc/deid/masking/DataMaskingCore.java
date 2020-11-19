/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.masking;

import java.io.IOException;
import java.util.List;
import com.ibm.whc.deid.providers.masking.MaskingProvider;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;
import com.ibm.whc.deid.shared.pojo.masking.ReferableData;

public class DataMaskingCore {

  private final MaskingProviderCache maskingProviderCache = new MaskingProviderCache();

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
      final List<ReferableData> inputData,
      ConfigSchemaTypes schemaType) throws IOException, DeidException {

    MaskingProvider complexMaskingProvider = maskingProviderCache.getMaskingProvider(configuration,
        null, schemaType);

    return protectRecord(inputData, complexMaskingProvider);
  }

}
