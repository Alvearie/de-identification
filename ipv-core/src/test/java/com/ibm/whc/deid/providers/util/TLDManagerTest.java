/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.util.TLDManager;

public class TLDManagerTest {
  @Test
  public void testTLD() {
    TLDManager tldManager = TLDManager.instance();
    String domain = "www.nba.com";
    assertTrue(tldManager.getTLD(domain).equals("com"));
    domain = "www.nba.co.uk";
    assertTrue(tldManager.getTLD(domain).equals("co.uk"));
  }

  @Test
  public void testGetRandomTLD() {
    TLDManager tldManager = TLDManager.instance();
    assertNotNull(tldManager.getRandomTLD());
    assertNotNull(tldManager.getRandomTLD(null));
  }

  @Test
  @Ignore
  public void testPerformanceGetTLD() {
    int N = 1000000;
    String hostname = "ie.ibm.com";
    TLDManager tldManager = TLDManager.instance();

    long startMillis = System.currentTimeMillis();
    for (int i = 0; i < N; i++) {
      tldManager.getTLD(hostname);
    }

    long diff = System.currentTimeMillis() - startMillis;
    System.out.println(String.format("%d operations took %d milliseconds (%f msec per op)", N, diff,
        (double) diff / N));
    // Assert test always should finish in less than 10 seconds
    assertTrue(diff < 10000);
  }
}
