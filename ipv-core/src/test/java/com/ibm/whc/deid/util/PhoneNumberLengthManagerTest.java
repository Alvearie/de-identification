/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.List;
import org.junit.Test;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.resources.KeyListResource;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.utils.log.LogCodes;

public class PhoneNumberLengthManagerTest implements MaskingProviderTest {

  @Test
  public void testBadInput() throws Exception {
    try {
      PhoneNumberLengthManager.buildPhoneNumberLengthManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=10, values=[43, 4-a]] from /localization/test.phone_number_digits.bad.csv: The value \"4-a\" for \"list resource value\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    String[] record = new String[] {"2", "98"};
    PhoneNumberLengthManager manager = new PhoneNumberLengthManager();
    PhoneNumberLengthManager.loadRecord(manager, record);
    KeyListResource<Integer> resource = manager.getValue("2");
    List<Integer> list = resource.getValue();
    assertNotNull(list);
    assertEquals(1, list.size());
    assertEquals(98, list.get(0).intValue());

    record = new String[] {"3", "4-6"};
    PhoneNumberLengthManager.loadRecord(manager, record);
    resource = manager.getValue("3");
    list = resource.getValue();
    assertNotNull(list);
    assertEquals(3, list.size());
    assertEquals(4, list.get(0).intValue());
    assertEquals(5, list.get(1).intValue());
    assertEquals(6, list.get(2).intValue());

    record = new String[] {"4", "7|9|11"};
    PhoneNumberLengthManager.loadRecord(manager, record);
    resource = manager.getValue("4");
    list = resource.getValue();
    assertNotNull(list);
    assertEquals(3, list.size());
    assertEquals(7, list.get(0).intValue());
    assertEquals(9, list.get(1).intValue());
    assertEquals(11, list.get(2).intValue());

    record = new String[] {"00$", "17"};
    PhoneNumberLengthManager.loadRecord(manager, record);
    resource = manager.getValue("00$");
    list = resource.getValue();
    assertNotNull(list);
    assertEquals(1, list.size());
    assertEquals(17, list.get(0).intValue());

    // trailing | is allowed
    record = new String[] {"5", "7|5|"};
    PhoneNumberLengthManager.loadRecord(manager, record);
    resource = manager.getValue("5");
    list = resource.getValue();
    assertNotNull(list);
    assertEquals(2, list.size());
    assertEquals(7, list.get(0).intValue());
    assertEquals(5, list.get(1).intValue());


    // bad input
    String temp = record[0];
    record[0] = null;
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource key"));
    }
    record[0] = " ";
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource key"));
    }
    record[0] = temp;

    temp = record[1];
    record[1] = null;
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource value"));
    }
    record[1] = "  ";
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource value"));
    }
    record[1] = "0";
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource value"));
    }
    record[1] = "-4";
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource value"));
    }
    record[1] = "5-";
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource value"));
    }
    record[1] = "aaa-4";
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("aaa"));
    }
    record[1] = "4-bbb";
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("bbb"));
    }
    record[1] = "-4-5";
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource value"));
    }
    record[1] = "0-5";
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource value"));
    }
    record[1] = "5-1";
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource value"));
    }
    record[1] = "5-0";
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource value"));
    }
    record[1] = "|5";
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource value"));
    }
    record[1] = "ccc|7";
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource value"));
    }
    record[1] = "7|10|ddd";
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource value"));
    }
    record[1] = "ccc|7";
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource value"));
    }
    record[1] = "8| |17";
    try {
      PhoneNumberLengthManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("resource value"));
    }
    record[1] = temp;
  }
}
