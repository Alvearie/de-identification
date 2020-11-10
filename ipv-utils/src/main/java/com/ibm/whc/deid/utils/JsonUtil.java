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
}
