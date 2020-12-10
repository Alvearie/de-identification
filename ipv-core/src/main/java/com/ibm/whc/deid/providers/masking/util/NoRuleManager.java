/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.util;

import com.fasterxml.jackson.databind.JsonNode;
import com.ibm.whc.deid.providers.masking.MaskingProvider;
import com.ibm.whc.deid.providers.masking.fhir.MaskingActionInputIdentifier;
import com.ibm.whc.deid.providers.masking.fhir.MaskingProviderBuilder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;

/**
 * Tracks the leaf nodes of a JSON document that are masked during masking processing so that
 * remaining leaf nodes can be processed by the provider designated for "defaultNoRuleRes".
 */
public class NoRuleManager {

  private MaskingProvider noRuleResProvider;

  private class JsonNodeMapWrapper {

    private final JsonNode node;

    public JsonNodeMapWrapper(JsonNode n) {
      node = n;
    }

    @Override
    public boolean equals(Object o) {
      return o instanceof JsonNodeMapWrapper && this.node == ((JsonNodeMapWrapper) o).node;
    }

    @Override
    public int hashCode() {
      return node.hashCode();
    }
  }

  private HashMap<JsonNodeMapWrapper, MaskingActionInputIdentifier> map = new HashMap<>(1000);

  public NoRuleManager(MaskingProviderBuilder.MaskingResource maskingResource, String resourceId,
      MaskingProvider noRuleResProvider) {
    this.noRuleResProvider = noRuleResProvider;
    findLeaves(maskingResource.getJsonNode(), "", maskingResource.getJsonNode(), resourceId,
        maskingResource.getResourceType());
  }

  public void removeNodesAlreadyMasked(List<MaskingActionInputIdentifier> listToMaskPerResource) {
    if (listToMaskPerResource != null) {
      for (MaskingActionInputIdentifier inputIdentifier : listToMaskPerResource) {
        JsonNode maskedNode = inputIdentifier.getNode();
        if (maskedNode != null) {
          map.remove(new JsonNodeMapWrapper(maskedNode));
        }
      }
    }
  }

  public void applyToRemainingNodes() {
    List<MaskingActionInputIdentifier> list = new ArrayList<>(map.values());
    noRuleResProvider.maskIdentifierBatch(list);
  }

  private void findLeaves(JsonNode rootNode, String parentPath, JsonNode parentNode,
      String resourceId, String resourceType) {
    if (parentNode != null && !parentNode.isNull()) {

      if (parentNode.isArray()) {
        int size = parentNode.size();
        for (int i = 0; i < size; i++) {
          JsonNode childNode = parentNode.get(i);
          if (!childNode.isNull()) {
            StringBuilder buffer = new StringBuilder(parentPath.length() + 10);
            buffer.append(parentPath).append('[').append(i).append(']');
            String childPath = buffer.toString();
            if (childNode.isArray() || childNode.isObject()) {
              findLeaves(rootNode, childPath, childNode, resourceId, resourceType);
            } else {
              map.put(new JsonNodeMapWrapper(childNode),
                  new MaskingActionInputIdentifier(noRuleResProvider, childNode, parentNode,
                      childPath, resourceType, resourceId, rootNode));
            }
          }
        }

      } else if (parentNode.isObject()) {
        Iterator<Entry<String, JsonNode>> it = parentNode.fields();
        while (it.hasNext()) {
          Entry<String, JsonNode> entry = it.next();
          JsonNode childNode = entry.getValue();
          if (childNode.isNull()) {
            continue;
          } else if (childNode.isArray() || childNode.isObject()) {
            findLeaves(rootNode, entry.getKey(), childNode, resourceId, resourceType);
          } else {
            map.put(new JsonNodeMapWrapper(childNode),
                new MaskingActionInputIdentifier(noRuleResProvider, childNode, parentNode,
                    entry.getKey(), resourceType, resourceId, rootNode));
          }
        }
      }
    }
  }
}
