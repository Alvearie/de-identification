/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.jsonpath;

import static org.junit.Assert.assertNotNull;

import com.fasterxml.jackson.databind.JsonNode;
import com.ibm.whc.deid.jsonpath.component.FieldComponent;
import org.junit.Test;

public class FieldComponentTest {

  private final String fieldName = "book";

  private final String testJson = "{ \"store\": {\n" + "    \"book\": [ \n"
      + "      { \"category\": \"reference\",\n" + "        \"author\": \"Nigel Rees\",\n"
      + "        \"title\": \"Sayings of the Century\",\n" + "        \"price\": 8.95\n"
      + "      },\n" + "      { \"category\": \"fiction\",\n"
      + "        \"author\": \"Evelyn Waugh\",\n" + "        \"title\": \"Sword of Honour\",\n"
      + "        \"price\": 12.99\n" + "      },\n" + "      { \"category\": \"fiction\",\n"
      + "        \"author\": \"Herman Melville\",\n" + "        \"title\": \"Moby Dick\",\n"
      + "        \"isbn\": \"0-553-21311-3\",\n" + "        \"price\": 8.99\n" + "      },\n"
      + "      { \"category\": \"fiction\",\n" + "        \"author\": \"J. R. R. Tolkien\",\n"
      + "        \"title\": \"The Lord of the Rings\",\n" + "        \"isbn\": \"0-395-19395-8\",\n"
      + "        \"price\": 22.99\n" + "      }\n" + "    ],\n" + "    \"bicycle\": {\n"
      + "      \"color\": \"red\",\n" + "      \"price\": \"19.95\",\n"
      + "      \"isRide\": true \n      }\n" + "  }\n" + "}";

  @Test
  public void testFieldComponentWithValidFieldName() throws Exception {
    JsonNode selection = JSONPathExtractor.extract(testJson, "/store");
    FieldComponent fieldComponent = new FieldComponent(fieldName);
    JsonNode jsonNode = fieldComponent.apply(selection);
    assertNotNull(jsonNode);
  }

  @Test(expected = JSONPathException.class)
  public void testFieldComponentWithInValidFieldName() throws Exception {
    JsonNode selection = JSONPathExtractor.extract(testJson, "/");
    FieldComponent fieldComponent = new FieldComponent(fieldName);
    fieldComponent.apply(selection);
  }
}
