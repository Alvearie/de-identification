/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.utils;

import static org.junit.Assert.*;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;

public class JsonUtilTest {

  private static ObjectMapper objectMapper = new ObjectMapper();
  private static String pathSeparator = ".";

  @Test
  public void testNullNode() {
    List<String> paths = new ArrayList<String>();
    JsonUtil.getPathsFromJsonNode(null, paths, "", JsonUtil.DEFAULT_PATH_SEPARATOR);
    assertNotNull(paths);
    assertEquals(0, paths.size());
  }

  @Test
  public void testNullPathList() {
    JsonNode schemaNode = null;
    try {
      schemaNode = objectMapper.readTree(this.getClass().getResourceAsStream("/Patient.json"));
    } catch (IOException e) {
      System.out.println("failed to load json: " + e);
    }
    List<String> paths = null;
    JsonUtil.getPathsFromJsonNode(schemaNode, paths, "", pathSeparator);
    assertNull(paths);
  }

  @Test
  public void testNullCurrentpath() {
    JsonNode schemaNode = null;
    try {
      schemaNode = objectMapper.readTree(this.getClass().getResourceAsStream("/Patient.json"));
    } catch (IOException e) {
      System.out.println("failed to load json: " + e);
    }
    List<String> paths = new ArrayList<String>();
    JsonUtil.getPathsFromJsonNode(schemaNode, paths, null, JsonUtil.DEFAULT_PATH_SEPARATOR);
    assertNotNull(paths);
    assertEquals(428, paths.size());
  }

  @Test
  public void testNullPathSeparator() {
    JsonNode schemaNode = null;
    try {
      schemaNode = objectMapper.readTree(this.getClass().getResourceAsStream("/Patient.json"));
    } catch (IOException e) {
      System.out.println("failed to load json: " + e);
    }
    List<String> paths = new ArrayList<String>();
    JsonUtil.getPathsFromJsonNode(schemaNode, paths, "", null);
    assertNotNull(paths);
    assertEquals(428, paths.size());
    boolean usesDefaultSeparator = false;
    for (String path : paths) {
      if (StringUtils.contains(path, JsonUtil.DEFAULT_PATH_SEPARATOR)) {
        usesDefaultSeparator = true;
        break;
      }
    }
    assertTrue(usesDefaultSeparator);
  }

  @Test
  public void testDeclaredPathSeparator() {
    JsonNode schemaNode = null;
    try {
      schemaNode = objectMapper.readTree(this.getClass().getResourceAsStream("/Patient.json"));
    } catch (IOException e) {
      System.out.println("failed to load json: " + e);
    }
    List<String> paths = new ArrayList<String>();
    JsonUtil.getPathsFromJsonNode(schemaNode, paths, "", pathSeparator);
    assertNotNull(paths);
    assertEquals(428, paths.size());
    boolean usesDeclaredSeparator = false;
    for (String path : paths) {
      if (StringUtils.contains(path, pathSeparator)) {
        usesDeclaredSeparator = true;
        break;
      }
    }
    assertTrue(usesDeclaredSeparator);
  }

  @Test
  public void testSchemaFromRoot() {
    JsonNode schemaNode = null;
    try {
      schemaNode = objectMapper.readTree(this.getClass().getResourceAsStream("/Patient.json"));
    } catch (IOException e) {
      System.out.println("failed to load json: " + e);
    }
    List<String> paths = new ArrayList<String>();
    JsonUtil.getPathsFromJsonNode(schemaNode, paths, "", JsonUtil.DEFAULT_PATH_SEPARATOR);
    assertNotNull(paths);
    assertEquals(428, paths.size());
  }

  @Test
  public void testDataFromRoot() {
    JsonNode dataNode = null;
    try {
      dataNode = objectMapper.readTree(this.getClass().getResourceAsStream("/patientExample.json"));
    } catch (IOException e) {
      System.out.println("failed to load json: " + e);
    }
    List<String> paths = new ArrayList<String>();
    JsonUtil.getPathsFromJsonNode(dataNode, paths, "", JsonUtil.DEFAULT_PATH_SEPARATOR);
    assertNotNull(paths);
    assertEquals(71, paths.size());
  }

  @Test
  public void testSchemaFromField() {
    JsonNode schemaNode = null;
    try {
      schemaNode = objectMapper.readTree(this.getClass().getResourceAsStream("/Patient.json"));
    } catch (IOException e) {
      System.out.println("failed to load json: " + e);
    }

    String fieldNameToTest = "identifier";
    List<String> paths = new ArrayList<String>();
    Iterator<Map.Entry<String, JsonNode>> fields = schemaNode.fields();
    while (fields.hasNext()) {
      Map.Entry<String, JsonNode> field = fields.next();
      if (field.getKey().equalsIgnoreCase(fieldNameToTest)) {
        JsonUtil.getPathsFromJsonNode(field.getValue(), paths, field.getKey(),
            JsonUtil.DEFAULT_PATH_SEPARATOR);
      }
    }
    assertNotNull(paths);
    assertEquals(16, paths.size());
    for (String path : paths) {
      assertTrue(StringUtils.startsWith(path, fieldNameToTest));
    }
  }

  @Test
  public void testDataFromField() {
    JsonNode dataNode = null;
    try {
      dataNode = objectMapper.readTree(this.getClass().getResourceAsStream("/patientExample.json"));
    } catch (IOException e) {
      System.out.println("failed to load json: " + e);
    }

    String fieldNameToTest = "identifier";
    List<String> paths = new ArrayList<String>();
    Iterator<Map.Entry<String, JsonNode>> fields = dataNode.fields();
    while (fields.hasNext()) {
      Map.Entry<String, JsonNode> field = fields.next();
      if (field.getKey().equalsIgnoreCase(fieldNameToTest)) {
        JsonUtil.getPathsFromJsonNode(field.getValue(), paths, field.getKey(),
            JsonUtil.DEFAULT_PATH_SEPARATOR);
      }
    }
    assertNotNull(paths);
    assertEquals(13, paths.size());
    for (String path : paths) {
      assertTrue(StringUtils.startsWith(path, fieldNameToTest));
    }
  }
}
