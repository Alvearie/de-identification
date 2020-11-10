/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.ibm.whc.deid.configuration.MaskingConfiguration;
import com.ibm.whc.deid.providers.masking.AbstractComplexMaskingProvider;
import com.ibm.whc.deid.providers.masking.DateTimeMaskingProvider;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateDependencyMaskingProviderConfig;

/**
 * Date dependency masking provider This complex masking provider is used as an intermediate step to
 * date time masking provider. It requires two date type fields in one resource type; one for
 * masking and the other one to compare with the masked value. This compared value is attached to
 * the masking configuration and sent to date time masking provider. The comparison operation is
 * done in date time masking provider
 *
 */
public class DateDependencyMaskingProvider extends AbstractComplexMaskingProvider<JsonNode> {

  private static final long serialVersionUID = 4938913531249878291L;

  private final String dateToMask;
  private final String dateToCompare;

  private final DateDependencyMaskingProviderConfig dateDependencyMaskingProviderConfig;

  /**
   * This is the constructor for date dependency masking provider
   *
   * @param maskingConfiguration : masking configuration for DateDependencyMaskingProvider
   */
  public DateDependencyMaskingProvider(String type, MaskingConfiguration maskingConfiguration) {
    super(type, maskingConfiguration);

    this.dateToMask =
        maskingConfiguration.getStringValue(type + ".datetime.year.delete.ninterval.maskdate");
    this.dateToCompare =
        maskingConfiguration.getStringValue(type + ".datetime.year.delete.ninterval.comparedate");

    this.dateDependencyMaskingProviderConfig = new DateDependencyMaskingProviderConfig();
  }

  public DateDependencyMaskingProvider(DateDependencyMaskingProviderConfig maskingConfiguration,
      DeidMaskingConfig deidMaskingConfig) {
    super(deidMaskingConfig);

    this.dateToMask = maskingConfiguration.getDatetimeYearDeleteNIntervalMaskDate();
    this.dateToCompare = maskingConfiguration.getDatetimeYearDeleteNIntervalCompareDate();

    this.dateDependencyMaskingProviderConfig = maskingConfiguration;
  }

  /**
   * This is the masking function for DateDependencyMaskingProvider
   *
   * @param node : contains an entire tree of the resource type
   * @return returning the updated node
   */
  public JsonNode mask(JsonNode node) {

    if (node == null || !node.has(dateToMask) || !node.has(dateToCompare)) {
      return null;
    }

    String maskDateValue = node.path(dateToMask).asText();
    String compareDateValue = node.path(dateToCompare).asText();

    // Add compared date value to the masking configuration for date time
    // masking provider process
    dateDependencyMaskingProviderConfig
        .setDatetimeyearDeleteNIntervalCompareDateValue(compareDateValue);

    // The rest of the configuration values, together with the compared
    // value, are passed down to date time masking provider
    DateTimeMaskingProvider maskingProvider =
        new DateTimeMaskingProvider(dateDependencyMaskingProviderConfig);
    String maskedValue = maskingProvider.mask(maskDateValue);

    // Update the field with the masked value returned from
    // DateTimeMaskingProvider
    ((ObjectNode) node).put(dateToMask, maskedValue);

    return node;
  }
}
