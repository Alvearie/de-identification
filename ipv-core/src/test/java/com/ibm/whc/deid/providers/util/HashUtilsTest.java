/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.HashSet;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.util.HashUtils;

public class HashUtilsTest {
  @Test
  public void testHashUtils() {
    HashUtils util = new HashUtils();
    assertNotNull(util);

    Long l = HashUtils.longFromHash("000000");
    assertNotNull(l);

    Long originalValue = l;
    for (int i = 0; i < 1000; i++) {
      l = HashUtils.longFromHash("000000");
      assertEquals(originalValue.longValue(), l.longValue());
    }
  }

  @Test
  public void testNull() {
    Long l = HashUtils.longFromHash(null);
    assertNotNull(l);
  }

  /**
   * Test 10,000 values and get their hash values. Use a HashSet to make sure they are all
   * different.
   */
  @Test
  public void testDifferentOriginalValue() {

    HashSet<Long> set = new HashSet<>();

    for (int i = 0; i < 10000; i++) {
      String str = String.format("%05d", i);
      long hash = HashUtils.longFromHash(str);

      set.add(hash);
    }

    assertEquals(10000, set.size());
  }

  @Test
  @Ignore
  public void testPerformance() {
    int N = 1000000;

    long start = System.currentTimeMillis();

    for (int i = 0; i < N; i++) {
      HashUtils.longFromHash("000000");
    }

    long diff = System.currentTimeMillis() - start;
    System.out.println("N: " + N + ", time: " + diff);
    // Assert test always should finish in less than 10 seconds
    assertTrue(diff < 10000);
  }
}
