/*
 * (C) Copyright IBM Corp. 2016,2021
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
}
