/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.jsonpath;

import java.io.IOException;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.BooleanNode;
import com.fasterxml.jackson.databind.node.DoubleNode;
import com.fasterxml.jackson.databind.node.IntNode;
import com.fasterxml.jackson.databind.node.JsonNodeType;
import com.fasterxml.jackson.databind.node.LongNode;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.TextNode;
import com.fasterxml.jackson.databind.node.ValueNode;

public final class JSONPathExtractor {

  private static final ObjectMapper mapper = new ObjectMapper();

  /** Given a json node and a json path, returns the value of the node */
  public static JsonNode extract(final JsonNode obj, final JSONPath pattern)
      throws JSONPathException {
    return pattern.apply(obj);
  }

  /** Given a json node and a string pattern to a json path, returns the value of the node */
  public static JsonNode extract(final JsonNode obj, final String pattern)
      throws JSONPathException {
    return extract(obj, JSONPath.compile(pattern));
  }

  /** Given a string node and a string pattern to a json path, returns the value of the node */
  public static JsonNode extract(final String objString, final String pattern)
      throws IOException, JSONPathException {
    return extract(mapper.readTree(objString), JSONPath.compile(pattern));
  }

  /** Given a string node, a string pattern to a json path, and a string value, update the node */
  public static JsonNode update(final String objString, final String pattern, String value)
      throws IOException, JSONPathException {
    return update(mapper.readTree(objString), JSONPath.compile(pattern), new TextNode(value));
  }

  /** Given a string node, a string pattern to a json path, and a long value, update the node */
  public static JsonNode update(final String objString, final String pattern, long value)
      throws IOException, JSONPathException {
    return update(mapper.readTree(objString), JSONPath.compile(pattern), new LongNode(value));
  }

  /** Given a string node, a string pattern to a json path, and a double value, update the node */
  public static JsonNode update(final String objString, final String pattern, double value)
      throws IOException, JSONPathException {
    return update(mapper.readTree(objString), JSONPath.compile(pattern), new DoubleNode(value));
  }

  /** Given a string node, a string pattern to a json path, and an integer value, update the node */
  public static JsonNode update(final String objString, final String pattern, int value)
      throws IOException, JSONPathException {
    return update(mapper.readTree(objString), JSONPath.compile(pattern), new IntNode(value));
  }

  /** Given the type of value, a node is created */
  private static ValueNode createNodeValue(JsonNode node) {
    JsonNodeType type = node.getNodeType();
    switch (type) {
      case STRING:
        return new TextNode(node.asText());
      case NUMBER:
        if (node.isDouble()) {
          return new DoubleNode(node.asDouble());
        } else if (node.isInt()) {
          return new IntNode(node.asInt());
        } else if (node.isLong()) {
          return new LongNode(node.asLong());
        }
        break;
      case BOOLEAN:
        return BooleanNode.valueOf(node.asBoolean());
      default:
        return NullNode.getInstance();
    }

    return NullNode.getInstance();
  }

  /** Given a string node, a string pattern to a json path, and json node, update the node */
  public static JsonNode update(final String objString, final String pattern, JsonNode node)
      throws IOException, JSONPathException {
    return update(mapper.readTree(objString), JSONPath.compile(pattern), createNodeValue(node));
  }

  /** Given a string node, a string pattern to a json path, and json node, update the value node */
  private static JsonNode update(JsonNode obj, JSONPath pattern, ValueNode value)
      throws JSONPathException {
    return pattern.update(obj, value);
  }
}
