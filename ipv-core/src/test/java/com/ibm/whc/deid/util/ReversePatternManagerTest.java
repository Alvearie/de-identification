/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.models.ReversePatternGenerator;
import java.security.SecureRandom;
import org.junit.Test;

public class ReversePatternManagerTest {
  private ReversePatternManager p = ReversePatternManager.getInstance();
  private SecureRandom random = new SecureRandom();

  @Test
  public void testLoadingArbitraryPatterns() {
    ReversePatternGenerator stringTest = p.getPatternByPattern("[abc]{20}");
    System.out.println(stringTest.getRandomToken(random));
    assertTrue(stringTest.getRandomToken(random).matches("^[abc]{20}$"));

    // Test that the pattern is cached
    int entries = p.getItemList().size();
    p.getPatternByPattern("[abc]{20}");
    assertTrue(entries == p.getItemList().size());
  }

  @Test
  public void testInvalidCountry() {
    ReversePatternGenerator wrongCountry = p.getPatternByResource("phoneNumber", "!!");
    assertTrue(wrongCountry == null);
  }

  @Test
  public void testInvalidPatternName() {
    ReversePatternGenerator wrongPattern = p.getPatternByResource("!!!!!!!", "us");
    assertTrue(wrongPattern == null);
  }
}
