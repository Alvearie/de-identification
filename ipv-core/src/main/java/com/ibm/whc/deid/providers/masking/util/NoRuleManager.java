/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import com.fasterxml.jackson.databind.JsonNode;
import com.ibm.whc.deid.providers.masking.MaskingProvider;
import com.ibm.whc.deid.providers.masking.fhir.MaskingActionInputIdentifier;
import com.ibm.whc.deid.providers.masking.fhir.MaskingProviderBuilder;

/**
 * Tracks the leaf nodes of a JSON document that are masked during masking processing so that
 * remaining leaf nodes can be processed by the provider designated for "defaultNoRuleRes".
 */
public class NoRuleManager {

  private MaskingProvider noRuleResProvider;

  private HashMap<NoRuleManagerHashKey, MaskingActionInputIdentifier> map = new HashMap<>(1000);

  public NoRuleManager(MaskingProviderBuilder.MaskingResource maskingResource, String resourceId,
      MaskingProvider noRuleResProvider) {
    this.noRuleResProvider = noRuleResProvider;
    findLeaves(maskingResource.getJsonNode(), "", maskingResource.getJsonNode(), resourceId,
        maskingResource.getResourceType());
  }

  public void removeNodesAlreadyMasked(List<MaskingActionInputIdentifier> listToMaskPerResource) {
    if (listToMaskPerResource != null) {
      for (MaskingActionInputIdentifier inputIdentifier : listToMaskPerResource) {
        map.remove(
            new NoRuleManagerHashKey(inputIdentifier.getParent(), inputIdentifier.getPath()));
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
              NoRuleManagerHashKey key = new NoRuleManagerHashKey(parentNode, childPath);
              if (map.containsKey(key)) {
                // this should not be possible
                throw new RuntimeException("NoRuleManager map already has incoming object key");
              }
              map.put(key, new MaskingActionInputIdentifier(noRuleResProvider, childNode,
                  parentNode, childPath, resourceType, resourceId, rootNode));
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
            NoRuleManagerHashKey key = new NoRuleManagerHashKey(parentNode, entry.getKey());
            if (map.containsKey(key)) {
              // this should not be possible
              throw new RuntimeException("NoRuleManager map already has incoming array key");
            }
            map.put(key, new MaskingActionInputIdentifier(noRuleResProvider, childNode, parentNode,
                entry.getKey(), resourceType, resourceId, rootNode));
          }
        }
      }
    }
  }
}
