/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.models.Race;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.utils.log.LogCodes;

public class RaceManagerTest implements MaskingProviderTest {

  @Test
  public void testBadInput() throws Exception {
    try {
      RaceManager.buildRaceManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=13, values=[ ]] from /localization/test.races.bad.csv: The value \" \" for \"race\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    RaceManager manager = new RaceManager();
    String locale = "us";
    String[] record = new String[] {"Race1"};
    RaceManager.loadRecord(locale, manager, record);
    Race resource = manager.getValue("racE1");
    assertNotNull(resource);
    assertEquals("Race1", resource.getName());
    assertSame(resource, manager.getValue(locale, "RACE1"));

    // bad name
    String temp = record[0];
    record[0] = null;
    try {
      RaceManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("race"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[0] = " ";
    try {
      RaceManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("race"));
    }
    record[0] = temp;

    // bad locale
    temp = locale;
    locale = null;
    try {
      RaceManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("race locale"));
      assertTrue(e.getMessage().contains("null"));
    }
    locale = " ";
    try {
      RaceManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("race locale"));
    }
    locale = temp;
  }
}
