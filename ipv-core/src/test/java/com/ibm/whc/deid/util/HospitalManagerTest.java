/*
 * (C) Copyright IBM Corp. 2021
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
import org.junit.Test;
import com.ibm.whc.deid.models.Hospital;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.utils.log.LogCodes;

public class HospitalManagerTest implements MaskingProviderTest {

  @Test
  public void testBadInput() throws Exception {
    try {
      HospitalManager.buildHospitalManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=35, values=[, US]] from /localization/test.hospital.bad.csv: The value \"\" for \"hospital name\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    String[] record = new String[] {"Olmsted County Hospital", "Canada"};
    HospitalManager manager = new HospitalManager();

    HospitalManager.loadRecord(manager, record);
    Hospital resource = manager.getValue("OLMsted county hospital");
    assertNotNull(resource);
    assertEquals("Olmsted County Hospital", resource.getName());
    assertEquals("Canada", resource.getNameCountryCode());
    assertSame(resource, manager.getValue("canada", "OLMsted county HOSPITAL"));
    assertNull(manager.getValue("us", "OLMsted county HOSPITAL"));

    // bad name
    String temp = record[0];
    record[0] = null;
    try {
      HospitalManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("hospital name"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[0] = " ";
    try {
      HospitalManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("hospital name"));
    }
    record[0] = temp;

    // bad country
    temp = record[0];
    record[1] = null;
    try {
      HospitalManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("hospital country"));
      assertTrue(e.getMessage().contains("null"));
    }
    record[1] = " ";
    try {
      HospitalManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("hospital country"));
    }
    record[1] = temp;
  }
}
