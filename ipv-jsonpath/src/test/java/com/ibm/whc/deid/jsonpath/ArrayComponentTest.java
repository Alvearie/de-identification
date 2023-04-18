/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.jsonpath;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import org.junit.Test;
import com.fasterxml.jackson.databind.JsonNode;
import com.ibm.whc.deid.jsonpath.component.ArrayComponent;

public class ArrayComponentTest {


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

  @Test(expected = NullPointerException.class)
  public void testArrayComponentNPE() {
    new ArrayComponent(null);
  }

  @Test
  public void testArrayComponent() throws Exception {
    JsonNode selection = JSONPathExtractor.extract(testJson, "/store");
    ArrayComponent arrayComponent = new ArrayComponent("book[2]");
    assertNotNull(arrayComponent);

    JsonNode newNode = arrayComponent.apply(selection);
    assertEquals("8.99", newNode.get("price").asText());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testArrayComponentWithInvalidPath() {
    new ArrayComponent("/store/invalidString");
  }

  @Test(expected = RuntimeException.class)
  public void testArrayComponentWithStar() throws Exception {
    JsonNode selection = JSONPathExtractor.extract(testJson, "/store");
    ArrayComponent arrayComponent = new ArrayComponent("/book[*]");
    assertNotNull(arrayComponent);

    arrayComponent.apply(selection);
  }

  @Test(expected = JSONPathException.class)
  public void testArrayComponentWithIncorrectArray() throws Exception {
    JsonNode selection = JSONPathExtractor.extract(testJson, "/store");
    ArrayComponent arrayComponent = new ArrayComponent("/books[3]");
    assertNotNull(arrayComponent);

    arrayComponent.apply(selection);
  }

  @Test
  public void testFieldComponentWithValidFieldName() throws Exception {
    assertNotNull(JSONPathExtractor.extract(testJson, "/store"));
  }
}
