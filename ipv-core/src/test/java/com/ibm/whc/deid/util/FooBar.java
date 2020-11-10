/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertNotNull;
import java.lang.reflect.InvocationTargetException;
import org.junit.Test;
/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

public class FooBar {

  private static final ObjectMapper mapper = new ObjectMapper();

  public static class A {
    public A(String[] s) {

    }

    public A(int[] o) {

    }
  }

  @Test
  public void checkJackson() throws Exception {
    String jsonString = "{\"a\":{\"b\":{\"c\":123}}}";

    JsonNode v = mapper.readTree(jsonString);

    JsonNode v1 = v.path("a.b.c");

    System.err.println(v1.asText());

    assertNotNull(v1.asText());
  }

  @Test
  public void test() throws NoSuchMethodException, IllegalAccessException,
      InvocationTargetException, InstantiationException {
    assertNotNull(
        A.class.getConstructor(String[].class).newInstance(new Object[] {new String[] {"foo"}}));
  }
}
