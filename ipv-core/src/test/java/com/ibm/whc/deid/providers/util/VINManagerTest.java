/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.VINManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class VINManagerTest {
  @Test
  public void testLookupSuccessful() throws Exception {
    VINManager vinManager = (VINManager) ManagerFactory.getInstance().getManager(null,
        Resource.WORLD_MANUFACTURERS_IDENTIFIER, null,
        LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);

    String wmi = "1C3";
    assertTrue(vinManager.isValidWMI(wmi));

    // check that the lowercase version is also matched
    wmi = "1c3";
    assertTrue(vinManager.isValidWMI(wmi));
  }

  @Test
  public void testInvalidWMI() throws Exception {
    VINManager vinManager = (VINManager) ManagerFactory.getInstance().getManager(null,
        Resource.WORLD_MANUFACTURERS_IDENTIFIER, null,
        LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);

    String wmi = "12";
    assertFalse(vinManager.isValidWMI(wmi));
  }

  @Test
  public void testGetRandomKey() throws Exception {
    VINManager vinManager = (VINManager) ManagerFactory.getInstance().getManager(null,
        Resource.WORLD_MANUFACTURERS_IDENTIFIER, null,
        LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);

    assertNotNull(vinManager.getRandomKey());
  }

  @Test
  public void testRandomWMIGenerator() throws Exception {
    VINManager vinManager =
        new VINManager(null, LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);
    // test random WMI

    for (int i = 0; i < 1000; i++) {
      assertTrue(vinManager.isValidWMI(vinManager.getRandomWMI()));
    }

    String exceptionWMI = "1C3";
    for (int i = 0; i < 1000; i++) {
      String randomWmi = vinManager.getRandomWMI(exceptionWMI);
      assertFalse(randomWmi.equals(exceptionWMI));
    }
  }
}
