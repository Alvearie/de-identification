/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import org.junit.Test;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class ATCManagerTest {

  @Test
  public void testCreate() {
    ATCManager mgr = ATCManager.buildATCManager(LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);
    assertNotNull(mgr);
    assertEquals(3681, mgr.getValues().size());
  } 
}
