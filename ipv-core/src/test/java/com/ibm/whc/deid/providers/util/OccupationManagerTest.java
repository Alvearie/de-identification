/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.util.OccupationManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class OccupationManagerTest {
  @Test
  public void testLookup() {
    OccupationManager occupationManager =
        new OccupationManager(null, LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);

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

    OccupationManager occupationManager =
        new OccupationManager(null, LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);
    for (String value : values) {
      assertEquals(Boolean.FALSE, Boolean.valueOf(occupationManager.isValidKey(value)));
    }
  }
}
