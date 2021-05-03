/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.models.County;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.utils.log.LogCodes;

public class CountyManagerTest implements MaskingProviderTest {

  @Test
  public void testBadInput() throws Exception {
    try {
      CountyManager.buildCountyManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=3, values=[Stutsman County, , North Dakota, 21120]] from /localization/test.county.bad.csv: The value \"\" for \"county short name\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    String[] record =
        new String[] {"Olmsted County", "Olmsted", "Minnesota", "189888"};
    CountyManager manager = new CountyManager();
    String locale = "us";

    CountyManager.loadRecord(locale, manager, record);
    County county = manager.getValue("OLMsted county");
    assertNotNull(county);
    assertEquals("Olmsted County", county.getName());
    assertEquals("Olmsted", county.getShortName());
    assertEquals("us", county.getNameCountryCode());
    assertEquals("OLMSTED COUNTY", county.getKey());
    assertTrue(county.isUseFullNameAsKey());
    county = manager.getValue("OLMsted");
    assertNotNull(county);
    assertEquals("Olmsted County", county.getName());
    assertEquals("Olmsted", county.getShortName());
    assertEquals("us", county.getNameCountryCode());
    assertEquals("OLMSTED", county.getKey());
    assertFalse(county.isUseFullNameAsKey());

    // bad name
    String temp = record[0];
    record[0] = null;
    try {
      CountyManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("county name"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[0] = " ";
    try {
      CountyManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("county name"));
    }
    record[0] = temp;

    // bad short name
    temp = record[1];
    record[1] = null;
    try {
      CountyManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("county short name"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[1] = "  ";
    try {
      CountyManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("county short name"));
    }
    record[1] = temp;

    // bad state
    temp = record[2];
    record[2] = null;
    try {
      CountyManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage(), e.getMessage().contains("county state"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[2] = "   ";
    try {
      CountyManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("county state"));
    }
    record[2] = temp;

    // bad population
    temp = record[3];
    record[3] = null;
    try {
      CountyManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("county population"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[3] = "   ";
    try {
      CountyManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("county population"));
    }
    record[3] = "axe";
    try {
      CountyManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("county population"));
      assertTrue(e.getMessage().contains("axe"));
    }
    record[3] = "-1";
    try {
      CountyManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("county population"));
      assertTrue(e.getMessage().contains("-1"));
    }

    // bad locale
    temp = locale;
    locale = null;
    try {
      CountyManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("county locale"));
      assertTrue(e.getMessage().contains("null"));
    }
    locale = " ";
    try {
      CountyManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("county locale"));
    }
    locale = temp;
  }
}
