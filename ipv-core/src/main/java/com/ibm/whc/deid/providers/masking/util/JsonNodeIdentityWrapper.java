/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.util;

import com.fasterxml.jackson.databind.JsonNode;

/**
 * Wrapper that serves to override equality for JsonNode objects so that equals()
 * is true only when compared to itself (==).  This allows nodes to be used in 
 * HashSets and as keys in HashMaps in situations where exact object matching is required.
 */
public class JsonNodeIdentityWrapper {

  private final JsonNode node;

  public JsonNodeIdentityWrapper(JsonNode n) {
    node = n;
  }

  @Override
  public boolean equals(Object o) {
    return o instanceof JsonNodeIdentityWrapper && this.node == ((JsonNodeIdentityWrapper) o).node;
  }

  @Override
  public int hashCode() {
    return node.hashCode();
  }
}
