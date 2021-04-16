/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.util.OccupationManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class OccupationManagerTest {

  @Test
  public void testLookup() {
    OccupationManager occupationManager = OccupationManager
        .buildOccupationManager(LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);

    String occupation = "actor";
    assertTrue(occupationManager.isValidKey(occupation));
    assertTrue(occupationManager.isValidKey(occupation.toLowerCase()));
    assertTrue(occupationManager.isValidKey(occupation.toUpperCase()));
    occupation = "acTor";
    assertTrue(occupationManager.isValidKey(occupation));
  }

  @Test
  public void testFalsePositives() {
    String[] values = {"C", "Z", "S", "P", "N", "G", "O", "-"};
    OccupationManager occupationManager = OccupationManager
        .buildOccupationManager(LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);
    for (String value : values) {
      assertFalse(occupationManager.isValidKey(value));
    }
  }
}
