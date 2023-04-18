/*
 * © Merative US L.P. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

import com.fasterxml.jackson.databind.JsonNode;

/**
 * Container that associates a JsonNode with its identifier.
 */
public class ReferableNode {

  private final String identifier;
  private final JsonNode node;

  public ReferableNode(String identifier, JsonNode node) {
    this.identifier = identifier;
    this.node = node;
  }

  public String getIdentifier() {
    return identifier;
  }

  public JsonNode getNode() {
    return node;
  }
}
