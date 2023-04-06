/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.etl.objects;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.ibm.whc.deid.util.Tuple;
import org.junit.Test;

public class RandomReplacementTest {
  private static final String ORIGINAL_STRING = "originalstring";
  private static final int MAX_TRIES = 10;

  @Test
  public void testApplyToEntireString() throws Exception {

    RandomReplacement rr = new RandomReplacement(0, ORIGINAL_STRING.length());
    for (int i = 0; i < MAX_TRIES; ++i) {
      Tuple<String, Boolean> tuple = rr.apply(ORIGINAL_STRING);
      assertNotNull(tuple);
      assertTrue(tuple.getSecond());
      assertNotNull(tuple.getFirst());
      if (!ORIGINAL_STRING.equalsIgnoreCase(tuple.getFirst())) {
        return;
      }
    }
    fail("random value never different than original value");
  }

  @Test
  public void testApplyToSubstring() throws Exception {

    RandomReplacement rr = new RandomReplacement(2, ORIGINAL_STRING.length() - 4);
    String subString = ORIGINAL_STRING.substring(2, ORIGINAL_STRING.length() - 2);

    boolean subStringChanged = false;
    for (int i = 0; i < MAX_TRIES; ++i) {
      Tuple<String, Boolean> tuple = rr.apply(ORIGINAL_STRING);
      assertNotNull(tuple);
      assertTrue(tuple.getSecond());
      assertNotNull(tuple.getFirst());
      assertEquals(subString.length(), tuple.getFirst().length());
      if (!subString.equalsIgnoreCase(tuple.getFirst())) {
        subStringChanged = true;
      }
    }
    assertTrue("random value never different than original value", subStringChanged);
  }

  @Test
  public void testApplyToSubstring2() throws Exception {

    RandomReplacement rr = new RandomReplacement(2, ORIGINAL_STRING.length());
    String subString = ORIGINAL_STRING.substring(2);

    boolean subStringChanged = false;
    for (int i = 0; i < MAX_TRIES; ++i) {
      Tuple<String, Boolean> tuple = rr.apply(ORIGINAL_STRING);
      assertNotNull(tuple);
      assertTrue(tuple.getSecond());
      assertNotNull(tuple.getFirst());
      assertEquals(subString.length(), tuple.getFirst().length());
      if (!subString.equalsIgnoreCase(tuple.getFirst())) {
        subStringChanged = true;
      }
    }
    assertTrue("random value never different than original value", subStringChanged);
  }

  @Test
  public void testApplyToNothing() throws Exception {

    RandomReplacement rr = new RandomReplacement(ORIGINAL_STRING.length(), 1);

    for (int i = 0; i < MAX_TRIES; ++i) {
      Tuple<String, Boolean> tuple = rr.apply(ORIGINAL_STRING);
      assertNotNull(tuple);
      assertTrue(tuple.getSecond());
      assertNotNull(tuple.getFirst());
      if (!ORIGINAL_STRING.equalsIgnoreCase(tuple.getFirst())) {
        fail("value change from original value");
      }
    }
  }
}
