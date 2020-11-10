/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.jsonpath;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

import com.fasterxml.jackson.core.JsonPointer;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.Test;

public class JSONPathExtractorTest {

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

  private final String validPattern = "/store/book/0/price";

  @Test
  public void testJSONPathExpressions() throws Exception {
    JsonNode selection = JSONPathExtractor.extract(testJson, validPattern);
    assertNotNull(selection);
    assertThat(selection.asDouble(), is(8.95));
  }

  @Test
  public void testUpdate() throws Exception {
    JsonNode selection = JSONPathExtractor.extract(testJson, validPattern);
    assertNotNull(selection);
    assertThat(selection.asDouble(), is(8.95));

    JsonNode update = JSONPathExtractor.update(testJson, validPattern, 10.001);

    assertNotNull(selection);
    assertThat(JSONPathExtractor.extract(update, validPattern).asDouble(), is(10.001));
  }

  @Test
  public void testUpdateWithDoubleValue() throws Exception {
    JsonNode update = JSONPathExtractor.update(testJson, validPattern, ((10.001)));
    assertThat(JSONPathExtractor.extract(update, validPattern).asDouble(), is(10.001));
  }

  @Test
  public void testUpdateWithStringValue() throws Exception {
    JsonNode update = JSONPathExtractor.update(testJson, validPattern, "10.001");
    assertThat(JSONPathExtractor.extract(update, validPattern).asText(), is("10.001"));
  }

  @Test
  public void testUpdateWithIntValue() throws Exception {
    JsonNode update = JSONPathExtractor.update(testJson, validPattern, ((10)));
    assertThat(JSONPathExtractor.extract(update, validPattern).asInt(), is(10));
  }

  @Test
  public void testUpdateWithLongValue() throws Exception {
    JsonNode update = JSONPathExtractor.update(testJson, validPattern, ((long) (10.001)));
    assertThat(JSONPathExtractor.extract(update, validPattern).asLong(), is(10L));
  }

  @Test
  public void testUpdateWithJSONNodeAsValue() throws Exception {
    JsonNode update = JSONPathExtractor.extract(testJson, "/store/book/1/price");
    JsonNode updatedNode = JSONPathExtractor.update(testJson, "/store/book/0/price", update);
    assertThat(JSONPathExtractor.extract(updatedNode, "/store/book/0/price").asDouble(), is(12.99));
  }

  @Test
  public void testUpdateWithJSONNodeAsStringValue() throws Exception {
    JsonNode update = JSONPathExtractor.extract(testJson, "/store/book/1/title");
    JsonNode updatedNode = JSONPathExtractor.update(testJson, "/store/book/0/title", update);
    assertThat(JSONPathExtractor.extract(updatedNode, "/store/book/0/title").asText(),
        is("Sword of Honour"));
  }

  @Test
  public void testUpdateWithJSONNodeAsBooleanValue() throws Exception {
    JsonNode update = JSONPathExtractor.extract(testJson, "/store/bicycle/isRide");
    JsonNode updatedNode = JSONPathExtractor.update(testJson, "/store/book/0/title", update);
    assertThat(JSONPathExtractor.extract(updatedNode, "/store/book/0/title").asBoolean(), is(true));
  }

  @Test
  public void test() throws Exception {
    ObjectMapper mapper = new ObjectMapper();

    JsonNode obj = mapper.readTree(testJson);

    JsonNode res = obj.at("/store/book");

    assertFalse(res.isMissingNode());

    JsonPointer.compile("/store/book/0/price");
  }
}
