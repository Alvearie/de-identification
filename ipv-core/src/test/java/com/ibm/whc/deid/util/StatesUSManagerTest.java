/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.models.State;
import com.ibm.whc.deid.models.StateNameFormat;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.utils.log.LogCodes;

public class StatesUSManagerTest implements MaskingProviderTest {

  @Test
  public void testBadInput() throws Exception {
    try {
      StatesUSManager.buildStatesUSManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=10, values=[Georgia, , 9992167]] from /localization/test.states_us.bad.csv: The value \"\" for \"state abbreviation\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    StatesUSManager manager = new StatesUSManager();
    String locale = "en";

    String[] record = new String[] {"South Dakota", "SD", "  "};
    StatesUSManager.loadRecord(locale, manager, record);
    State resource = manager.getValue("south dakota");
    assertNotNull(resource);
    assertEquals("South Dakota", resource.getName());
    assertEquals(StateNameFormat.FULL_NAME, resource.getNameFormat());
    assertEquals(locale, resource.getNameCountryCode());
    resource = manager.getValue("sd");
    assertNotNull(resource);
    assertEquals("South Dakota", resource.getName());
    assertEquals(StateNameFormat.ABBREVIATION, resource.getNameFormat());
    assertEquals(locale, resource.getNameCountryCode());

    record = new String[] {"North Dakota", "ND"};
    StatesUSManager.loadRecord(locale, manager, record);
    resource = manager.getValue("north dakota");
    assertNotNull(resource);
    assertEquals("North Dakota", resource.getName());
    assertEquals(StateNameFormat.FULL_NAME, resource.getNameFormat());
    assertEquals(locale, resource.getNameCountryCode());
    resource = manager.getValue("nd");
    assertNotNull(resource);
    assertEquals("North Dakota", resource.getName());
    assertEquals(StateNameFormat.ABBREVIATION, resource.getNameFormat());
    assertEquals(locale, resource.getNameCountryCode());

    // bad name
    String temp = record[0];
    record[0] = null;
    try {
      StatesUSManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("null"));
      assertTrue(e.getMessage().contains("state name"));
    }
    record[0] = " ";
    try {
      StatesUSManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("state name"));
    }
    record[0] = temp;

    // bad abbreviation
    temp = record[1];
    record[1] = null;
    try {
      StatesUSManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("null"));
      assertTrue(e.getMessage().contains("state abbreviation"));
    }
    record[1] = " ";
    try {
      StatesUSManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("state abbreviation"));
    }
    record[1] = temp;

    // bad locale
    temp = locale;
    locale = null;
    try {
      StatesUSManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("state locale"));
      assertTrue(e.getMessage().contains("null"));
    }
    locale = " ";
    try {
      StatesUSManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("state locale"));
    }
    locale = temp;
  }
}
