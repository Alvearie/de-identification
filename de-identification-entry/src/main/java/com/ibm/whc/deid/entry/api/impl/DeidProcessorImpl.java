/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.entry.api.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import com.ibm.whc.deid.entry.api.DeidProcessor;
import com.ibm.whc.deid.providers.masking.BasicMaskingProviderFactory;
import com.ibm.whc.deid.providers.masking.ComplexMaskingProvider;
import com.ibm.whc.deid.providers.masking.ComplexMaskingProviderFactoryUtil;
import com.ibm.whc.deid.shared.exception.InvalidInputException;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.ReferableData;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import com.ibm.whc.deid.shared.util.MaskingConfigUtils;

public class DeidProcessorImpl implements DeidProcessor {

  private final DeidMaskingConfig maskingConfig;
  private final ComplexMaskingProvider complexMaskingProvider;

  public DeidProcessorImpl(final DeidMaskingConfig maskingConfiguration)
      throws InvalidMaskingConfigurationException {
    validateInput(maskingConfiguration);
    this.maskingConfig = maskingConfiguration;
    BasicMaskingProviderFactory maskingProviderFactory = new BasicMaskingProviderFactory();
    ConfigSchemaType schemaType = maskingConfig.getJson().getSchemaType();
    this.complexMaskingProvider =
        ComplexMaskingProviderFactoryUtil.getComplexMaskingProviderFactory().get(schemaType,
            maskingConfig, maskingProviderFactory, null);
  }

  @Override
  public List<String> process(List<String> input) {
    validateData(input);
    List<ReferableData> refData = addIdentifiers(input);
    List<ReferableData> output = complexMaskingProvider.maskWithBatch(refData, "external");
    List<String> response = removeIdentifiers(output);
    return response;
  }

  private List<ReferableData> addIdentifiers(List<String> data) {
    AtomicInteger messageOrder = new AtomicInteger();
    ArrayList<ReferableData> refData = new ArrayList<>(data.size());
    for (String record : data) {
      refData.add(new ReferableData(String.valueOf(messageOrder.getAndIncrement()), record));
    }
    return refData;
  }

  private List<String> removeIdentifiers(List<ReferableData> output) {
    ArrayList<String> out = new ArrayList<>(output.size());
    for (ReferableData record : output) {
      out.add(record.getData());
    }
    return out;
  }

  protected void validateInput(final DeidMaskingConfig maskingConfiguration)
      throws InvalidMaskingConfigurationException, IllegalArgumentException {
    MaskingConfigUtils.getInstance().validateConfigObject(maskingConfiguration);
  }

  protected void validateData(List<String> data) {
    // validates data is non-null and non-empty. Validates individual strings for non-null as well.
    if (data == null || data.isEmpty()) {
      throw new IllegalArgumentException(new InvalidInputException("data"));
    }
    int i = 0;
    for (String d : data) {
      if (d == null) {
        throw new IllegalArgumentException(new InvalidInputException("data[" + i + "]"));
      }
      i++;
    }
  }
}
