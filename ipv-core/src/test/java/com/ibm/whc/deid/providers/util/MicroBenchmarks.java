/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertTrue;
import org.apache.commons.lang3.StringUtils;
import org.junit.Ignore;
import org.junit.Test;

public class MicroBenchmarks {
  @Test
  @Ignore
  public void testLevenshtein() {
    int N = 1000000;
    int listSize = 1000;
    long startMillis = System.currentTimeMillis();

    for (int i = 0; i < N; i++) {
      for (int j = 0; j < listSize; j++) {
        StringUtils.getLevenshteinDistance("horse", "harse");
      }
    }

    long diff = System.currentTimeMillis() - startMillis;
    System.out
        .println(String.format("String.format: %d operations took %d milliseconds (%f per op)", N,
            diff, (double) diff / N));
    // System.out.println("distance is: " + distance);
    // Assert test always should finish in less than 3 minutes
    assertTrue(diff < 180000);
  }

  @Test
  @Ignore
  public void testStringFormatVSBuilder() {
    int N = 1000000;
    String host = "hostname";
    String protocol = "http";
    String file = "index.php";

    long startMillis = System.currentTimeMillis();

    for (int i = 0; i < N; i++) {
      String.format("%s://%s/%s", protocol, host, file);
    }

    long diff = System.currentTimeMillis() - startMillis;
    System.out
        .println(String.format("String.format: %d operations took %d milliseconds (%f per op)", N,
            diff, (double) diff / N));

    startMillis = System.currentTimeMillis();

    for (int i = 0; i < N; i++) {
      StringBuilder builder = new StringBuilder(protocol);
      builder.append("://");
      builder.append(host);
      builder.append("/");
      builder.append(file);
      builder.toString();
    }

    diff = System.currentTimeMillis() - startMillis;
    System.out
        .println(String.format("StringBuilder: %d operations took %d milliseconds (%f per op)", N,
            diff, (double) diff / N));
    // Assert test always should finish in less than 10 seconds
    assertTrue(diff < 10000);
  }
}
