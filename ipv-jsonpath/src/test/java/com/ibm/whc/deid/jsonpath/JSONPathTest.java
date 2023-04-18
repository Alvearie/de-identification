/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.jsonpath;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class JSONPathTest {

  private final String validPattern = "/store/book/0/price";

  private final String invalidPattern = "invalid string";

  @Test(expected = NullPointerException.class)
  public void testInitialJSONPathWithNullPattern() {
    JSONPath.compile(null);
  }

  @Test
  public void testJSONPathExpressionByValidPattern() throws Exception {
    assertTrue(JSONPath.isValid(validPattern));
  }

  @Test
  public void testJSONPathExpressionByInvalidPattern() throws Exception {
    assertFalse(JSONPath.isValid(invalidPattern));
  }
}
