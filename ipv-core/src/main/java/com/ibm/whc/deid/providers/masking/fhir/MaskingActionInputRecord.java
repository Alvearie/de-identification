/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;

public class MaskingActionInputRecord {

  String avroId;

  String resourceId;

  String[] brokenDownPathElements;

  FHIRResourceMaskingAction maskingAction;

  String fullPath;

  JsonNode unmaskedNode;

  List<MaskingActionInputIdentifier> identifiers = new ArrayList<>();

  public String getResourceId() {
    return resourceId;
  }

  public void setResourceId(String resourceId) {
    this.resourceId = resourceId;
  }

  public String[] getBrokenDownPathElements() {
    return brokenDownPathElements;
  }

  public void setBrokenDownPathElements(String[] brokenDownPathElements) {
    this.brokenDownPathElements = brokenDownPathElements;
  }

  public FHIRResourceMaskingAction getMaskingAction() {
    return maskingAction;
  }

  public void setMaskingAction(FHIRResourceMaskingAction maskingAction) {
    this.maskingAction = maskingAction;
  }

  public String getFullPath() {
    return fullPath;
  }

  public void setFullPath(String fullPath) {
    this.fullPath = fullPath;
  }

  public JsonNode getUnmaskedNode() {
    return unmaskedNode;
  }

  public void setUnmaskedNode(JsonNode unmaskedNode) {
    this.unmaskedNode = unmaskedNode;
  }

  public String getAvroId() {
    return avroId;
  }

  public void setAvroId(String avroId) {
    this.avroId = avroId;
  }
}
