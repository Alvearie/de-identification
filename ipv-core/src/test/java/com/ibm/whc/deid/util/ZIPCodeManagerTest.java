/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.Arrays;
import java.util.HashSet;
import org.junit.Test;
import com.ibm.whc.deid.models.ZIPCode;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.utils.log.LogCodes;

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
    assertEquals(testDataPop, zipCodeManager.getPopulationByPrefix("us", "5590"));
    assertEquals(Integer.valueOf(0), zipCodeManager.getPopulationByPrefix("us", "00000"));
    assertNull(zipCodeManager.getPopulationByPrefix(null, "5590"));
    assertNull(zipCodeManager.getPopulationByPrefix("us", null));
    assertEquals(Integer.valueOf(0), zipCodeManager.getPopulationByPrefix("en", "5590"));
    assertEquals(Integer.valueOf(0), zipCodeManager.getPopulationByPrefix("#$@", "559"));
    assertEquals(Integer.valueOf(0), zipCodeManager.getPopulationByPrefix("us", "!@$@%%"));

    HashSet<String> candidates = new HashSet<>(
        Arrays.asList("58001", "58002", "58004", "58005", "58006", "58007", "58008", "58009"));
    for (int i = 0; i < 40; i++) {
      assertTrue(candidates.contains(zipCodeManager.getRandomZipCodeByPrefix("us", "5800")));
    }
    assertEquals("58001", zipCodeManager.getRandomZipCodeByPrefix("us", "58001"));
    assertNull(zipCodeManager.getRandomZipCodeByPrefix("en", "580"));
    assertNull(zipCodeManager.getRandomZipCodeByPrefix("us", "0000"));
    assertNull(zipCodeManager.getRandomZipCodeByPrefix(null, "5800"));
    assertNull(zipCodeManager.getRandomZipCodeByPrefix("us", null));

    assertTrue(zipCodeManager.isValidKey("US", "00601"));
    assertTrue(zipCodeManager.isValidKey("us", "55901"));

    assertEquals(13508, zipCodeManager.getValue("99901").getPopulation());
  }

  @Test
  public void testBadInput() throws Exception {
    try {
      ZIPCodeManager.buildZIPCodeManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=8, values=[58048, unknown]] from /localization/test.zcta.bad.csv: The value \"unknown\" for \"ZIPcode population\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    ZIPCodeManager manager = new ZIPCodeManager();
    String locale = "us";
    String[] record = new String[] {"55901", "125000"};
    ZIPCodeManager.loadRecord(locale, manager, record);
    ZIPCode resource = manager.getValue("55901");
    assertNotNull(resource);
    assertEquals("55901", resource.getCode());
    assertEquals(125000, resource.getPopulation());
    assertSame(resource, manager.getValue(locale, "55901"));

    // bad code
    String temp = record[0];
    record[0] = null;
    try {
      ZIPCodeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ZIPcode"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[0] = " ";
    try {
      ZIPCodeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ZIPcode"));
    }
    record[0] = temp;

    // bad population
    temp = record[1];
    record[1] = null;
    try {
      ZIPCodeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ZIPcode population"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[1] = "   ";
    try {
      ZIPCodeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ZIPcode population"));
    }
    record[1] = "axe";
    try {
      ZIPCodeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ZIPcode population"));
      assertTrue(e.getMessage().contains("axe"));
    }
    record[1] = "-1";
    try {
      ZIPCodeManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("ZIPcode population"));
      assertTrue(e.getMessage().contains("-1"));
    }
  }
}
