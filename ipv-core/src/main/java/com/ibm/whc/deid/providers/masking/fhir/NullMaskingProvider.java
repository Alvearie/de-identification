/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.NullNode;
import com.ibm.whc.deid.configuration.MaskingConfiguration;
import com.ibm.whc.deid.providers.masking.AbstractComplexMaskingProvider;

public class NullMaskingProvider extends AbstractComplexMaskingProvider<JsonNode> {

  private static final long serialVersionUID = -2703814679554294365L;

  /**
   * @param type
   * @param maskingConfiguration
   * @param fullPath
   */
  public NullMaskingProvider(String type, MaskingConfiguration maskingConfiguration,
      String fullPath) {
    super(type, maskingConfiguration);
  }

  public NullMaskingProvider() {}

  public JsonNode mask(JsonNode node) {
    return NullNode.getInstance();
  }
}
