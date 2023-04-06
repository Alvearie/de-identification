/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import org.junit.Ignore;
import org.junit.Test;

public class DateTimeIdentifierTest {
  @Test
  public void testIsOfThisType() throws Exception {
    DateTimeIdentifier identifier = new DateTimeIdentifier();

    String[] validDatetimes =
        {"08-12-1981 00:00:11", "08/12/1981 00:00:11", "08-12-1981", "1981-12-08",};

    String[] invalidDatetimes = {"foobar", "08-13-1981 00:00:00", "01-01-1981 27:05:22", ""};

    for (String date : validDatetimes) {
      assertTrue(identifier.isOfThisType(date));
    }

    for (String date : invalidDatetimes) {
      System.out.println(date);
      assertFalse(identifier.isOfThisType(date));
    }
  }

  @Test
  @Ignore
  public void testPerformance() {
    DateTimeIdentifier identifier = new DateTimeIdentifier();

    int N = 1000000;
    String[] validDatetimes =
        {"08-12-1981 00:00:11", "08/12/1981 00:00:11", "08-12-1981", "1981-12-08",};

    for (String originalDateTime : validDatetimes) {
      long startMillis = System.currentTimeMillis();

      for (int i = 0; i < N; i++) {
        identifier.isOfThisType(originalDateTime);
      }

      long diff = System.currentTimeMillis() - startMillis;
      System.out.printf("%s: %d operations took %d milliseconds (%f per op)", originalDateTime, N,
          diff, (double) diff / N);
      // Assert test always should finish in less than 10 seconds
      assertTrue(diff < 10000);
    }
  }

  @Test
  public void fromDemoFile() throws Exception {
    DateTimeIdentifier identifier = new DateTimeIdentifier();

    try (BufferedReader br = new BufferedReader(
        new InputStreamReader(this.getClass().getResourceAsStream("/demo.csv")))) {
      for (String line = br.readLine(); null != line; line = br.readLine()) {
        final String value = line.split(",")[0];
        try {
          assertTrue(value, identifier.isOfThisType(value));
          System.out.println("Yes: " + value);
        } catch (AssertionError e) {
          System.out.println(" No: " + e.getMessage());
        }
      }
    }
  }
}
