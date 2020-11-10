/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.masking;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;
import com.ibm.whc.deid.providers.masking.MaskingProvider;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;
import scala.Tuple2;

public class DataMaskingCore {

  private final MaskingProviderCache maskingProviderCache = new MaskingProviderCache();

  /**
   * @param input
   * @param maskingProvider
   * @param maskingOptions
   * @return
   */
  protected List<Tuple2<String, String>> protectRecord(List<Tuple2<String, String>> input,
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
  public List<String> maskData(final String configuration, final List<String> inputData,
      ConfigSchemaTypes schemaType) throws IOException, DeidException {

    MaskingProvider complexMaskingProvider = maskingProviderCache.getMaskingProvider(configuration,
        null, schemaType);

    return protectRecord(inputData.stream().map(record -> new Tuple2<String, String>("", record))
        .collect(Collectors.toList()), complexMaskingProvider).stream()
            .map(record -> {
              return record._2();
            }).collect(Collectors.toList());
  }

}
