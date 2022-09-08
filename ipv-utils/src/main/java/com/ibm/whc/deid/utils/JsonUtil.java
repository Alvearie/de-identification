/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.utils;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.apache.commons.lang3.StringUtils;

public class JsonUtil {

  public static final String DEFAULT_PATH_SEPARATOR = "/";

  /**
   * Extract the field/element paths from the Json preserving the full path for parent/child
   * relationship.
   *
   * @param jsonNode The JsonNode object to process
   * @param paths The List to store the path results. If null, no results are returned.
   * @param currentPath The current Json path. If processing the root Json node, use "" or null. If
   *        processing from a child Json node, use the child path name.
   * @param pathSeparator The value to use to separate the path parts. If not specified the "/" is
   *        used as default.
   */
  public static void getPathsFromJsonNode(JsonNode jsonNode, List<String> paths, String currentPath,
      String pathSeparator) {
    if (jsonNode == null)
      return;
    if (currentPath == null)
      currentPath = "";
    if (StringUtils.isEmpty(pathSeparator))
      pathSeparator = DEFAULT_PATH_SEPARATOR;

    // Add the path to the list of paths
    addJsonPath(paths, currentPath, pathSeparator);

    if (jsonNode.isValueNode()) {
      addJsonPath(paths, currentPath, pathSeparator);
    } else if (jsonNode.isContainerNode() && !jsonNode.isObject()) {
      Iterator<JsonNode> elements = jsonNode.elements();
      while (elements.hasNext()) {
        JsonNode element = elements.next();
        getPathsFromJsonNode(element, paths, currentPath, pathSeparator);
      }
    } else if (jsonNode.isObject()) {
      Iterator<Map.Entry<String, JsonNode>> fields = jsonNode.fields();
      while (fields.hasNext()) {
        Map.Entry<String, JsonNode> field = fields.next();
        getPathsFromJsonNode(field.getValue(), paths, currentPath + pathSeparator + field.getKey(),
            pathSeparator);
        addJsonPath(paths, currentPath, pathSeparator);
      }
    }
  }

  private static void addJsonPath(List<String> pathList, String pathToAdd, String pathSeparator) {
    if (pathToAdd == null || pathList == null)
      return;
    if (StringUtils.startsWith(pathToAdd, pathSeparator))
      pathToAdd = StringUtils.stripStart(pathToAdd, pathSeparator);
    if (!StringUtils.isEmpty(pathToAdd) && !pathList.contains(pathToAdd))
      pathList.add(pathToAdd);
  }

  /**
   * Returns the value of the given JSON Value (leaf) node.
   *
   * @param node the node from which the value is extracted
   * 
   * @return <i>Null</i> if the given node is <i>null</i> or
   * is not a Value node (leaf node), an Optional containing 
   * an object that represents the value contained in the node or
   * an empty Optional if the node contains an explicit <i>null</i>
   * value.
   */
  public static Optional<Object> getValue(JsonNode node) {
    Optional<Object> value = null;
    if (node != null && node.isValueNode()) {
      if (node.isNull()) {
        value = Optional.empty();
      } else if (node.isBoolean()) {
        value = Optional.of(node.booleanValue());
      } else if (node.isShort()) {
        value = Optional.of(node.shortValue());        
      } else if (node.isInt()) {
        value = Optional.of(node.intValue());
      } else if (node.isLong()) {
        value = Optional.of(node.longValue());
      } else if (node.isFloat()) {
        value = Optional.of(node.floatValue());
      } else if (node.isDouble()) {
        value = Optional.of(node.doubleValue());
      } else if (node.isBigInteger()) {
        value = Optional.of(node.bigIntegerValue());
      } else if (node.isBigDecimal()) {
        value = Optional.of(node.decimalValue());
      } else {
        value = Optional.of(node.asText());
      }
    }
    return value;
  }

  private JsonUtil() {
    // no need to instantiate
  }
}
