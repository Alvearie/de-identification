/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import com.fasterxml.jackson.databind.JsonNode;
import com.ibm.whc.deid.providers.masking.fhir.MaskingActionInputIdentifier;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.ConditionalMaskRuleSet;
import com.ibm.whc.deid.shared.pojo.config.masking.ConditionalMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition;

/**
 * The type Conditional masking provider.
 *
 */
public class ConditionalMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -13830496083023321L;

  private static final String ERROR_MESSAGE = "A Conditional node value was unparsable";

  private final List<ConditionalMaskRuleSet> maskRuleSet;
  private final DeidMaskingConfig deidMaskingConfig;
  private final MaskingProviderFactory maskingProviderFactory;

  public ConditionalMaskingProvider(ConditionalMaskingProviderConfig configuration, String tenantId,
      DeidMaskingConfig deidMaskingConfig, String localizationProperty,
      MaskingProviderFactory maskingProviderFactory) {
    super(tenantId, localizationProperty, configuration);
    this.deidMaskingConfig = deidMaskingConfig;
    this.maskRuleSet = configuration.getMaskRuleSet();
    this.maskingProviderFactory = maskingProviderFactory;
  }

  @Override
  public void maskIdentifierBatch(List<MaskingActionInputIdentifier> identifiers) {
    for (MaskingActionInputIdentifier i : identifiers) {
      String value = mask(i);
      putField(i, value);
    }
  }

  /**
   * Masks the specified identifier by the masking provider of the condition that is met.
   *
   * @param the input to be masked
   * 
   * @return the updated value as a string
   */
  protected String mask(MaskingActionInputIdentifier maii) {
    JsonNode root = maii.getRoot();
    String resourceType = maii.getResourceType();
    String identifier = maii.getNode().asText();

    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    if (maskRuleSet.isEmpty()) {
      // No masking rules provided.
      return null;
    }

    return maskRuleSet.stream().filter(rule -> {
      return evaluateConditionTypeString(identifier, rule, root, resourceType);
    }).findFirst().map(rule -> {
      MaskingProvider maskingProvider = getMaskingProvider(rule.getMaskingProvider());
      return maskingProvider.mask(identifier);
    }).orElse(identifier);
  }

  private MaskingProvider getMaskingProvider(MaskingProviderConfig config) {
    return maskingProviderFactory.getProviderFromType(config.getType(), deidMaskingConfig, config,
        tenantId, localizationProperty);
  }

  /**
   * Evaluate the condition to see if it matches
   *
   * @param identifier
   * @param conditionalMaskRuleSet
   * @return
   */
  private boolean evaluateConditionTypeString(String identifier,
      ConditionalMaskRuleSet conditionalMaskRuleSet, JsonNode root, String resourceType) {

    Condition condition = conditionalMaskRuleSet.getCondition();
    if (condition == null) {
      // If there are no condition, but a masking provider is configured,
      // just return true so that masking provider is used.
      return conditionalMaskRuleSet.getMaskingProvider() != null;
    }

    Set<String> valueSet;
    if (condition.getField().contains("==")) {
      // Array query path
      valueSet = getConditionArrayFieldValue(condition, root, resourceType);
    } else {
      // Regular path
      valueSet = getConditionRegularFieldValues(condition, root, resourceType);
    }
    boolean conditionMatch = false;
    String conditionValue = condition.getValue();

    for (String value : valueSet) {
      switch (condition.getOperator()) {
        case EQUALS:
          if (value.equals(conditionValue)) {
            return true;
          }
          break;
        case EQUALS_IGNORE_CASE:
          if (value.equalsIgnoreCase(conditionValue)) {
            return true;
          }
          break;
        case CONTAINS:
          if (value.contains(conditionValue)) {
            return true;
          }
          break;
        case CONTAINED_IN:
          if (conditionValue.contains(value)) {
            return true;
          }
          break;
      }
    }
    return conditionMatch;
  }

  private Set<String> getConditionRegularFieldValues(Condition condition, JsonNode root,
      String resourceType) {

    Set<String> valueSet = new HashSet<>();

    String path = condition.getField();
    String[] paths = null;
    if (path.startsWith("/")) {
      paths = path.substring(1).split("/");
    } else {
      paths = path.split("/");
    }

    /*
     * We go through two loops: The first loop goes through the paths elements to get to the leaf
     * node that matches the path, if path is not found, it returns an empty valueSet.
     */
    String currentPath = "";
    List<JsonNode> nodeList = new ArrayList<>();
    for (int i = 0; i < paths.length; i++) {
      /*
       * The Following code for each paths.i traverses the nodes and collects the child nodes, then
       * at the end sets nodeList to childNodeList which for the next loop nodeList becomes the new
       * parent.
       */

      currentPath = paths[i];
      // System.out.println("========> " + i + " currentPath: " +
      // currentPath);
      nodeList = getChildrenNodes(root, currentPath, nodeList);
      if (nodeList == null) {
        return valueSet;
      }
    }

    for (JsonNode elementNode : nodeList) {
      // System.out.println("\n\n========> elementNode: " +
      // elementNode.toString());
      if (elementNode.isValueNode()) {
        Object value = getValue(elementNode);
        if (value == null) {
          throw new RuntimeException(ERROR_MESSAGE);
        }
        valueSet.add(value.toString());
      } else {
        Iterator<Entry<String, JsonNode>> dataNodeList = elementNode.fields();
        while (dataNodeList.hasNext()) {
          Entry<String, JsonNode> entryNode = dataNodeList.next();
          String key = entryNode.getKey();
          JsonNode valueNode = entryNode.getValue();
          if (key.equalsIgnoreCase(condition.getValue())) {
            // System.out.println(" NextNode=======> key : " + key +
            // " valuNode: " + valueNode.toString());
            Object value = getValue(valueNode);
            if (value == null) {
              throw new RuntimeException(ERROR_MESSAGE);
            }
            valueSet.add(value.toString());
          }
        }
      }
    }
    return valueSet;
  }

  /**
   * @param condition the condition to be examined
   * @param root the root of the document being processed
   * 
   * @return the set of value(s) of a condition field
   */
  private Set<String> getConditionArrayFieldValue(Condition condition, JsonNode root,
      String resourceType) {

    Set<String> valueSet = new HashSet<>();

    String path = condition.getField();
    String[] paths = null;
    int eqeqIndex = path.indexOf("==");
    if (eqeqIndex > 0) {
      if (path.startsWith("/")) {
        paths = path.substring(1, eqeqIndex).split("/");
      } else {
        paths = path.substring(0, eqeqIndex).split("/");
      }
      paths[paths.length - 1] = paths[paths.length - 1] + path.substring(eqeqIndex);
    } else {
      if (path.startsWith("/")) {
        paths = path.substring(1).split("/");
      } else {
        paths = path.split("/");
      }
    }

    // System.out.println("\n\n========> paths: " + Arrays.toString(paths));

    /*
     * Currently, only array leaf nodes and a single condition is supported. This path is for array
     * nodes with query conditions. For example, a FullPath of the form :
     * "/telecom/value(system==phone)"
     */
    String currentPath = "";
    String conditionName = null;
    String conditionValue = null;
    String valueName = null;
    List<String> dataPathList = new ArrayList<>();

    /*
     * We go through two loops: The first loop goes through the paths elements to get to the leaf
     * node with the condition, and then determines the specified conditionName and conditionValue.
     */
    List<JsonNode> nodeList = new ArrayList<>();
    for (int i = 0; i < paths.length; i++) {

      /*
       * Goes through each paths elements to collect all the array nodes and the condition name and
       * value.
       */
      if (paths[i].contains("==")) {
        // This the paths element that has the condition, get the
        // condition name and value
        String[] arrayElementAndCondition = paths[i].split("\\(");

        String arrayElement = arrayElementAndCondition[0].trim();
        String conditionELement = arrayElementAndCondition[1].trim().replace(")", "");
        String[] pathCondition = conditionELement.split("==");
        conditionName = pathCondition[0].trim();
        conditionValue = pathCondition[1].trim();
        valueName = arrayElement;

        // System.out.println("\n\n========> conditionName: " +
        // conditionName);
        // System.out.println("\n\n========> conditionValue: " +
        // conditionValue);
        // System.out.println("\n\n========> valueName: " + valueName);

        dataPathList.add(arrayElement);
      } else {
        /*
         * The Following code for each paths.i traverses the nodes and collects the child nodes,
         * then at the end sets nodeList to childNodeList which for the next loop nodeList becomes
         * the new parent.
         */
        currentPath = paths[i];

        nodeList = getChildrenNodes(root, currentPath, nodeList);
        if (nodeList == null) {
          return valueSet;
        }
      }
    }
    /*
     * The second loop walks through the array resource node to find the leaf node with sibling that
     * matches the condition name and value determined in the first loop above. Note: the
     * conditionName of * and conditionValue of * are used as a wild card. *
     */
    // JsonNode arrayNode = node.get(arrayPath);

    // JsonNode arrayNode = getNodeFromPath(arrayPath, node);
    // for (JsonNode elementNode : arrayNode) {
    for (JsonNode elementNode : nodeList) {
      if (elementNode.has(conditionName)) {
        // System.out.println("\n\n========> elementNode: " +
        // elementNode.toString());
        String elementConditionValue = elementNode.get(conditionName).textValue();
        if (elementConditionValue.equals(conditionValue) || ("*".equals(conditionValue))) {
          Iterator<Entry<String, JsonNode>> dataNodeList = elementNode.fields();
          while (dataNodeList.hasNext()) {
            Entry<String, JsonNode> entryNode = dataNodeList.next();
            String key = entryNode.getKey();
            JsonNode valueNode = entryNode.getValue();
            if (key.equalsIgnoreCase(valueName)) {
              // System.out.println(" NextNode=======> key : " +
              // key + " valuNode: " + valueNode.toString());

              Object value = getValue(valueNode);
              if (value == null) {
                throw new RuntimeException(ERROR_MESSAGE);
              }
              valueSet.add(value.toString());

              return valueSet;
            }
          }
        }
      }
    }
    return valueSet;
  }

  /**
   * getChildrenNodes - get children nodes of the nodeList for the given path.
   *
   * @param node
   * @param currentPath
   * @param nodeList
   * @return nodeList (new node list)
   */
  private List<JsonNode> getChildrenNodes(JsonNode node, String currentPath,
      List<JsonNode> nodeList) {
    if (nodeList.isEmpty()) {
      JsonNode currentNode = node.get(currentPath);

      if (currentNode == null || currentNode.isNull()) {
        // The FHIR resource node does not exist.
        // The rule does not apply.
        return null;
      }
      if (currentNode.isArray()) {
        for (JsonNode childNode : currentNode) {
          nodeList.add(childNode);
          // System.out.println("========> " +
          // " nodeList.add(childNode): " + childNode);
        }
      } else {
        nodeList.add(currentNode);
        // System.out.println("========> " +
        // " nodeList.add(currentNode): " + currentNode);
      }
    } else {

      List<JsonNode> childNodeList = new ArrayList<>();
      for (JsonNode subNode : nodeList) {
        JsonNode currentNode = subNode.get(currentPath);
        // System.out.println("========> " + " currentPath: " +
        // currentPath);
        // System.out.println("========> " + " subNode: " +
        // subNode.toString());
        if (currentNode == null || currentNode.isNull()) {
          continue;
        }

        if (currentNode.isArray()) {
          for (JsonNode childNode : currentNode) {
            childNodeList.add(childNode);
            // System.out.println("========> " +
            // " childNodeList.add(childNode): " +
            // childNode.toString());
          }
        } else {
          childNodeList.add(currentNode);
          // System.out.println("========> " +
          // " childNodeList.add(currentNode): " + currentNode);
        }
      }
      nodeList = childNodeList;
    }

    return nodeList;
  }

  /**
   * getValue - returns the value of the given JSON ValueNode.
   *
   * @param value
   * @return
   */
  private Object getValue(JsonNode value) {
    if (value == null || value.isNull()) {
      return null;
    }
    if (value.isBoolean())
      return value.asBoolean();
    if (value.isShort() || value.isInt() || value.isIntegralNumber())
      return value.asInt();
    if (!value.isTextual() || value.textValue() == null) {
      return value.toString();
    }
    return value.textValue();
  }

  @Override
  public String mask(String identifier) {
    throw new IllegalStateException("should not be reached");
  }
}
