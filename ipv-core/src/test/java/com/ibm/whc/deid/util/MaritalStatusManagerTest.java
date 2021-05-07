/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.models.County;
import com.ibm.whc.deid.models.MaritalStatus;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.util.MaritalStatusManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.utils.log.LogCodes;

public class MaritalStatusManagerTest implements MaskingProviderTest {

  @Test
  public void testLookup() throws Exception {
    MaritalStatusManager maritalStatusManager =
        MaritalStatusManager.buildMaritalStatusManager(localizationProperty);
    String status = "Single";
    assertTrue(maritalStatusManager.isValidKey(status));

    status = "singLE";
    assertTrue(maritalStatusManager.isValidKey(status));

    status = "xxx";
    assertFalse(maritalStatusManager.isValidKey(status));
  }

  @Test
  public void testRandomCodeGenerator() throws Exception {
    MaritalStatusManager maritalStatusManager =
        MaritalStatusManager.buildMaritalStatusManager(localizationProperty);
    assertTrue(maritalStatusManager.isValidKey(maritalStatusManager.getRandomKey()));
  }

  @Test
  public void testBadInput() throws Exception {
    try {
      MaritalStatusManager.buildMaritalStatusManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=4, values=[  ]] from /localization/test.marital_status.bad.csv: The value \"  \" for \"marital status\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    String locale = "us";
    String[] record = new String[] {"not-married"};
    MaritalStatusManager manager = new MaritalStatusManager();
    MaritalStatusManager.loadRecord(locale, manager, record);
    MaritalStatus resource = manager.getValue("NOT-Married");
    assertNotNull(resource);
    assertEquals("not-married", resource.getName());
    assertSame(resource, manager.getValue(locale, "not-MARRIED"));

    // bad name
    String temp = record[0];
    record[0] = null;
    try {
      MaritalStatusManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("marital status"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[0] = " ";
    try {
      MaritalStatusManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("marital status"));
    }
    record[0] = temp;

    // bad locale
    temp = locale;
    locale = null;
    try {
      MaritalStatusManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("marital status locale"));
      assertTrue(e.getMessage().contains("null"));
    }
    locale = " ";
    try {
      MaritalStatusManager.loadRecord(locale, manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("marital status locale"));
    }
    locale = temp;
  }
}
