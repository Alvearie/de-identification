/*
 * © Merative US L.P. 2016,2021
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
import com.ibm.whc.deid.models.WorldManufacturerId;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.utils.log.LogCodes;

public class VINManagerTest implements MaskingProviderTest {

  @Test
  public void testIsValidWMI() throws Exception {
    VINManager vinManager = (VINManager) ManagerFactory.getInstance().getManager(null,
        Resource.WORLD_MANUFACTURERS_IDENTIFIER, null,
        LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);

    String wmi = "1C3";
    assertTrue(vinManager.isValidWMI(wmi));

    // check that the lowercase version is also matched
    wmi = "1c3";
    assertTrue(vinManager.isValidWMI(wmi));

    wmi = "12";
    assertFalse(vinManager.isValidWMI(wmi));

    wmi = null;
    assertFalse(vinManager.isValidWMI(wmi));
  }

  @Test
  public void testGetRandomKey() throws Exception {
    VINManager vinManager = (VINManager) ManagerFactory.getInstance().getManager(null,
        Resource.WORLD_MANUFACTURERS_IDENTIFIER, null,
        LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);

    for (int i = 0; i < 100; i++) {
      String random = vinManager.getRandomKey();
      assertNotNull(random);
      assertTrue(vinManager.isValidKey(random));
    }
  }

  @Test
  public void testBadInput() throws Exception {
    try {
      VINManager.buildVINManager(ERROR_LOCALIZATION_PROPERTIES);
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertEquals(
          "Invalid values were encountered while reading record CSVRecord [comment='null', recordNumber=2, values=[1E , Fleetwood]] from /localization/test.vin_wmi.bad.csv: The value \"1E \" for \"WMI\" is invalid",
          e.getMessage());
    }
  }

  @Test
  public void testLoadRecord() {
    String[] record = new String[] {"1f6", "Ford"};
    VINManager manager = new VINManager();

    VINManager.loadRecord(manager, record);
    WorldManufacturerId wmi = manager.getValue("1f6");
    assertNotNull(wmi);
    assertEquals("1F6", wmi.getId());
    assertEquals("Ford", wmi.getManufacturer());

    // bad WMI
    String temp = record[0];
    record[0] = null;
    try {
      VINManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("WMI"));
    }
    record[0] = "";
    try {
      VINManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("WMI"));
    }
    record[0] = "   ";
    try {
      VINManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("WMI"));
    }
    record[0] = "8u";
    try {
      VINManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("WMI"));
    }
    record[0] = "1325";
    try {
      VINManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("WMI"));
    }
    record[0] = "132 ";
    try {
      VINManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("WMI"));
    }
    record[0] = temp;

    // bad manufacturer
    temp = record[1];
    record[1] = null;
    try {
      VINManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("manufacturer"));
    }
    record[1] = "  ";
    try {
      VINManager.loadRecord(manager, record);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getMessage().contains("manufacturer"));
    }
    record[1] = temp;
  }
}
