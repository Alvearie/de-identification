/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.jsonpath;

import com.fasterxml.jackson.core.JsonPointer;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.ValueNode;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import org.apache.commons.lang3.StringUtils;

public final class JSONPath implements Serializable {

  /**
   * 
   */
  private static final long serialVersionUID = -1092223094403555377L;
  private final String pattern;
  private static LogManager log = LogManager.getInstance();

  private JSONPath(final String pattern) {
    if (null == pattern) {
      log.logError(LogCodes.WPH1010E, "null", "jsonPath pattern");
      throw new NullPointerException(LogCodes.WPH1010E + "null" + "jsonPath pattern");
    }

    this.pattern = pattern;
  }

  /** Checks if input string has correct json format */
  public static boolean isValid(final String pattern) {
    try {
      JsonPointer.compile(pattern);

      return true;
    } catch (Exception ignored) {
    }

    return false;
  }

  public static JSONPath compile(final String pattern) {
    return new JSONPath(pattern);
  }

  public JsonNode apply(JsonNode obj) {
    return obj.at(pattern);
  }

  /** Updates Json Node given a node value */
  public JsonNode update(JsonNode obj, ValueNode value) throws JSONPathException {
    List<String> list = Arrays.asList(pattern.split("/"));
    JsonNode node;
    if (list.size() > 2) { // because of "" before the first / and the
      // second one will be the field we want to
      // modify
      String newPattern = StringUtils.join(list.subList(0, list.size() - 1), "/");

      node = obj.at(newPattern);
    } else {
      node = obj.at("");
    }

    if (node.isArray()) {
      ((ArrayNode) node).set(Integer.parseInt(list.get(list.size() - 1), 10), value);
    } else if (node.isObject()) {
      ((ObjectNode) node).set(list.get(list.size() - 1), value);
    }

    return obj;
  }
}
