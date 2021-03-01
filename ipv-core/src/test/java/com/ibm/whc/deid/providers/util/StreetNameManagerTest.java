/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.util.StreetNameManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class StreetNameManagerTest {

  private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

  @Test
  public void testLookupSuccessful() throws Exception {
    StreetNameManager streetNameManager = new StreetNameManager(null, localizationProperty);

    String streetName = "Woodland";
    assertTrue(streetNameManager.isValidKey(streetName));

    // case checking
    streetName = "WooDLand";
    assertTrue(streetNameManager.isValidKey(streetName));
  }

  @Test
  public void testRandomKeySuccessful() throws Exception {
    StreetNameManager streetNameManager = new StreetNameManager(null, localizationProperty);

    String streetName = "Woodland";
    String randomStreetName = streetNameManager.getRandomKey();

    assertFalse(randomStreetName.equals(streetName));
  }
}
