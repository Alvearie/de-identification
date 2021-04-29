/*
 * (C) Copyright IBM Corp. 2021
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

public class PhoneCountryCodesManagerTest implements MaskingProviderTest {

  @Test
  public void testBadInput() throws Exception {
    try {
      PhoneCountryCodesManager.buildPhoneCountryCodesManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=4, values=[684, ]] from /localization/test.phone_calling_codes.bad.csv: The value \"\" for \"resource value\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    String[] record =
        new String[] {"key1", "value1"};
    PhoneCountryCodesManager manager = new PhoneCountryCodesManager();
    PhoneCountryCodesManager.loadRecord(manager, record);
    manager.isValidKey("key1");

    // bad input
    String temp = record[0];
    record[0] = null;
    try {
      PhoneCountryCodesManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource key"));
    }
    record[0] = " ";
    try {
      PhoneCountryCodesManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource key"));
    }
    record[0] = temp;

    temp = record[1];
    record[1] = null;
    try {
      PhoneCountryCodesManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource value"));
    }
    record[1] = "  ";
    try {
      PhoneCountryCodesManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource value"));
    }
    record[1] = temp;
  }
}
