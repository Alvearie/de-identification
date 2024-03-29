/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import com.fasterxml.jackson.databind.JsonNode;
import com.ibm.whc.deid.providers.masking.MaskingProvider;

public class MaskingActionInputIdentifier {

  private MaskingProvider provider;
  private JsonNode node;
  private JsonNode parent;
  private String path;
  private String resourceId;
  private String resourceType;
  private JsonNode root;
  private JsonNode currentNode;
  
  public MaskingActionInputIdentifier(MaskingProvider provider, JsonNode node, JsonNode parent,
      String path, String resourceType, String resourceId, JsonNode root) {
    this.provider = provider;
    this.node = node;
    this.parent = parent;
    this.path = path;
    this.resourceId = resourceId;
    this.resourceType = resourceType;
    this.root = root;
  }

  public MaskingProvider getProvider() {
    return provider;
  }

  public void setProvider(MaskingProvider provider) {
    this.provider = provider;
  }

  public JsonNode getNode() {
    return node;
  }

  public void setNode(JsonNode node) {
    this.node = node;
  }

  public JsonNode getParent() {
    return parent;
  }

  public void setParent(JsonNode parent) {
    this.parent = parent;
  }

  public String getPath() {
    return path;
  }

  public void setPath(String path) {
    this.path = path;
  }

  public String getResourceId() {
    return resourceId;
  }

  public void setResourceId(String resourceId) {
    this.resourceId = resourceId;
  }

  public String getResourceType() {
    return resourceType;
  }

  public void setResourceType(String resourceType) {
    this.resourceType = resourceType;
  }

  public JsonNode getRoot() {
    return root;
  }

  public void setRoot(JsonNode root) {
    this.root = root;
  }

  /**
   * Retrieves the updated node or the original node if no update
   * has been recorded
   * 
   * @return the current node value 
   */
  public JsonNode getCurrentNode() {
    return currentNode == null ? node : currentNode;
  }

  /**
   * @param currentNode the currentNode to set
   */
  public void setCurrentNode(JsonNode currentNode) {
    this.currentNode = currentNode;
  }
}
