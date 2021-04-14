/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import java.util.HashSet;
import org.junit.Test;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.util.ZIPCodeManager;

public class ZIPCodeManagerTest implements MaskingProviderTest {

  @Test
  public void testDefault() {
    ZIPCodeManager zipCodeManager = ZIPCodeManager.buildZIPCodeManager(localizationProperty);
    testDefault(zipCodeManager);
  }

  protected void testDefault(ZIPCodeManager zipCodeManager) {
    assertEquals(5, zipCodeManager.getZipCodeLength("uS"));
    assertEquals("000", zipCodeManager.getZipCodeReplacement("uS"));
    assertEquals(5, zipCodeManager.getZipCodeLength("Us"));
    assertEquals("000", zipCodeManager.getZipCodeReplacement("Us"));

    assertEquals(-1, zipCodeManager.getZipCodeLength("en"));
    assertEquals("", zipCodeManager.getZipCodeReplacement("en"));

    assertEquals(-1, zipCodeManager.getZipCodeLength("ca"));
    assertEquals("", zipCodeManager.getZipCodeReplacement("ca"));

    assertEquals(-1, zipCodeManager.getZipCodeLength(null));
    assertEquals("", zipCodeManager.getZipCodeReplacement(null));

    assertEquals(-1, zipCodeManager.getZipCodeLength(""));
    assertEquals("", zipCodeManager.getZipCodeReplacement(""));

    Integer testDataPop = new Integer(51917 + 21968 + 26332 + 17598 + 1428);
    assertEquals(testDataPop, zipCodeManager.getPopulationByPrefix("us", "5590X", 4));
    assertEquals(testDataPop, zipCodeManager.getPopulationByPrefix("us", "5590", 4));
    assertEquals(testDataPop, zipCodeManager.getPopulationByPrefix("us", "559000", 4));
    assertNull(zipCodeManager.getPopulationByPrefix("us", "559", 4));
    assertNull(zipCodeManager.getPopulationByPrefix("us", "55901", 0));
    assertNull(zipCodeManager.getPopulationByPrefix("us", "55901", -1));
    assertEquals(Integer.valueOf(0), zipCodeManager.getPopulationByPrefix("us", "00000", 5));
    assertNull(zipCodeManager.getPopulationByPrefix(null, "5590", 4));
    assertEquals(Integer.valueOf(0), zipCodeManager.getPopulationByPrefix("en", "5590", 4));
    assertEquals(Integer.valueOf(0), zipCodeManager.getPopulationByPrefix("#$@", "00601", 3));
    assertEquals(Integer.valueOf(0), zipCodeManager.getPopulationByPrefix("US", "!@$@%%", 3));

    HashSet<String> candidates = new HashSet<>(
        Arrays.asList("58001", "58002", "58004", "58005", "58006", "58007", "58008", "58009"));
    for (int i = 0; i < 40; i++) {
      assertTrue(candidates.contains(zipCodeManager.getRandomZipCodeByPrefix("us", "58003", 4)));
    }

    assertTrue(zipCodeManager.isValidKey("US", "00601"));
    assertTrue(zipCodeManager.isValidKey("us", "55901"));

    assertEquals(13508, zipCodeManager.getValue("99901").getPopulation());
  }
}
