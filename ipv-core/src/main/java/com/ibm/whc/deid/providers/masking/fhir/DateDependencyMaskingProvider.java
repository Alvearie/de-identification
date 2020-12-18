/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import java.util.List;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.ibm.whc.deid.providers.masking.AbstractComplexMaskingProvider;
import com.ibm.whc.deid.providers.masking.DateTimeMaskingProvider;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateDependencyMaskingProviderConfig;

/**
 * Date dependency masking provider This complex masking provider is used as an intermediate step to
 * date time masking provider. It requires two date type fields in the same JSON object; one for
 * masking and the other one to compare with the masked value. This compared value is attached to
 * the masking configuration and sent to date time masking provider. The comparison operation is
 * done in date time masking provider.
 */
public class DateDependencyMaskingProvider extends AbstractComplexMaskingProvider<JsonNode> {

  private static final long serialVersionUID = 4938913531249878291L;

  private final String dateToCompare;

  private final DateDependencyMaskingProviderConfig dateDependencyMaskingProviderConfig;

  public DateDependencyMaskingProvider(DateDependencyMaskingProviderConfig maskingConfiguration,
      DeidMaskingConfig deidMaskingConfig) {
    super(deidMaskingConfig);
    this.dateToCompare = maskingConfiguration.getDatetimeYearDeleteNIntervalCompareDate();
    this.dateDependencyMaskingProviderConfig = maskingConfiguration;
  }

  @Override
  public void maskIdentifierBatch(List<MaskingActionInputIdentifier> identifiers) {
    for (MaskingActionInputIdentifier i : identifiers) {
      mask(i.getParent(), i.getPath());
    }
  }

  /**
   * This is the masking function for DateDependencyMaskingProvider.
   *
   * @param node the parent JSON object node which is expected to contain the property
   *        being masked and the property to which the current value of the property being masked is
   *        compared.
   * 
   * @param dateToMask the name of the property being masked
   * 
   * @return the updated node
   */
  protected JsonNode mask(JsonNode node, String dateToMask) {    
    if (node != null && node.has(dateToMask) && node.has(dateToCompare)) {      
      JsonNode maskNode = node.get(dateToMask);
      JsonNode compareNode = node.get(dateToCompare);      
      if (!maskNode.isNull() && !compareNode.isNull()) {
        
        String maskDateValue = maskNode.asText();
        String compareDateValue = compareNode.asText();

        // The configuration values and the compare date value are passed to datetime masking provider
        DateTimeMaskingProvider maskingProvider =
            new DateTimeMaskingProvider(dateDependencyMaskingProviderConfig, compareDateValue);
        String maskedValue = maskingProvider.mask(maskDateValue);
    
        // Update the field with the masked value returned from
        // DateTimeMaskingProvider
        ((ObjectNode) node).put(dateToMask, maskedValue);
      }
    }  
    
    return node;
  }
}
