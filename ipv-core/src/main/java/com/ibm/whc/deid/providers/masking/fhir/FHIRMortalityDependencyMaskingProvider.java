/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import java.util.List;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.ibm.whc.deid.providers.masking.AbstractMaskingProvider;
import com.ibm.whc.deid.providers.masking.DateTimeMaskingProvider;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateDependencyMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.FHIRMortalityDependencyMaskingProviderConfig;

public class FHIRMortalityDependencyMaskingProvider extends AbstractMaskingProvider {
  
  private static final long serialVersionUID = -3890925036519369771L;

  private final int minYears;

  public FHIRMortalityDependencyMaskingProvider(
      FHIRMortalityDependencyMaskingProviderConfig maskingConfiguration,
      DeidMaskingConfig deidMaskingConfig) {
    this.minYears = maskingConfiguration.getMortalityIndicatorMinYears();
  }

  @Override
  public void maskIdentifierBatch(List<MaskingActionInputIdentifier> identifiers) {
    for (MaskingActionInputIdentifier i : identifiers) {
      String target = i.getPath();
      JsonNode node = i.getNode();
      if ("deceasedDateTime".equals(target)) {
        
        if (!node.isNull()) {
          if (Boolean.valueOf(node.asText())) {
            //TODO: get deceasedBoolean and set true
          }
          putField(i, null);
            
      
        for (MaskingActionInputIdentifier i : identifiers) {
          String value = i.getNode().asText();
          value = mask(value);
          putField(i, value);
        }

      mask(i.getParent(), i.getPath());
    }
  }

  @Override
  public String mask(String identifier) {
    return null;
  }

  /**
   * This is the masking function for DateDependencyMaskingProvider.
   *
   * @param node the parent JSON object node which is expected to contain the property being masked
   *        and the property to which the current value of the property being masked is compared.
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

        // The configuration values and the compare date value are passed to datetime masking
        // provider
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
