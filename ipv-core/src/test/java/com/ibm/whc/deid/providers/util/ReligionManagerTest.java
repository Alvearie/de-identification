/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.util.ReligionManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class ReligionManagerTest {
	private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

  @Test
  public void testLookupSuccessful() throws Exception {
    ReligionManager religionManager = new ReligionManager(null, localizationProperty);
    String religion = "Catholic";
    assertTrue(religionManager.isValidKey(religion));

    religion = "caTHolic";
    assertTrue(religionManager.isValidKey(religion));
  }

  @Test
  public void testRandomCodeGenerator() throws Exception {
    ReligionManager religionManager = new ReligionManager(null, localizationProperty);
    assertTrue(religionManager.isValidKey(religionManager.getRandomKey()));
  }
}
