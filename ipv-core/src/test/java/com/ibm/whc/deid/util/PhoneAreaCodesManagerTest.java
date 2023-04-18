/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.utils.log.LogCodes;

public class PhoneAreaCodesManagerTest implements MaskingProviderTest {

  @Test
  public void testBadInput() throws Exception {
    try {
      PhoneAreaCodesManager.buildPhoneAreaCodesManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=2, values=[USA, 9x7, Alaska]] from /localization/test.phone_area_codes.bad.csv: The value \"9x7\" for \"area code\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    String[] record = new String[] {"us", "98"};
    PhoneAreaCodesManager manager = new PhoneAreaCodesManager();
    PhoneAreaCodesManager.loadRecord(manager, record);
    PhoneAreaCodeResource code = manager.getValue("us", "98");
    assertEquals("98", code.getAreaCode());

    // bad input
    String temp = record[0];
    record[0] = null;
    try {
      PhoneAreaCodesManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("country code"));
    }
    record[0] = " ";
    try {
      PhoneAreaCodesManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("country code"));
    }
    record[0] = temp;

    temp = record[1];
    record[1] = null;
    try {
      PhoneAreaCodesManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("area code"));
    }
    record[1] = "  ";
    try {
      PhoneAreaCodesManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("area code"));
    }
    record[1] = "axe";
    try {
      PhoneAreaCodesManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("axe"));
      assertTrue(e.getMessage().contains("area code"));
    }
    record[1] = "-2";
    try {
      PhoneAreaCodesManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("-2"));
      assertTrue(e.getMessage().contains("area code"));
    }
    record[1] = temp;
  }
}
