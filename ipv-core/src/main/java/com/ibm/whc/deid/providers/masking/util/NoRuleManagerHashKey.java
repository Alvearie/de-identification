/*
 * © Merative US L.P. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.util;

import com.fasterxml.jackson.databind.JsonNode;

public class NoRuleManagerHashKey {

  private final JsonNode parent;
  private final String pathInParent;
  private final int hashcode;

  public NoRuleManagerHashKey(JsonNode n, String path) {
    parent = n;
    pathInParent = path;
    hashcode = 31 * System.identityHashCode(parent) + pathInParent.hashCode();
  }

  public JsonNode getJsonNode() {
    return parent;
  }

  @Override
  public boolean equals(Object o) {
    return o instanceof NoRuleManagerHashKey && this.parent == ((NoRuleManagerHashKey) o).parent
        && this.pathInParent.equals(((NoRuleManagerHashKey) o).pathInParent);
  }

  @Override
  public int hashCode() {
    return hashcode;
  }
}
