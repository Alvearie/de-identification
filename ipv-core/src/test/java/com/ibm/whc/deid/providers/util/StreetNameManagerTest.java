/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.util.StreetNameManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class StreetNameManagerTest {

  private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

  @Test
  public void testMain() throws Exception {
    StreetNameManager streetNameManager =
        StreetNameManager.buildStreetNameManager(localizationProperty);

    String streetName = "Woodland";
    assertTrue(streetNameManager.isValidKey(streetName));
    assertEquals("Woodland", streetNameManager.getValue(streetName).getValue());

    // case checking
    streetName = "WooDLand";
    assertTrue(streetNameManager.isValidKey(streetName));
    assertEquals("Woodland", streetNameManager.getValue(streetName).getValue());
    
    // locale
    assertTrue(streetNameManager.isValidKey("uS", streetName));
    assertEquals("Woodland", streetNameManager.getValue(streetName).getValue());

    // not in locale
    assertFalse(streetNameManager.isValidKey("en", streetName));

    // not found
    streetName = "XX";
    assertNull(streetNameManager.getValue(streetName));
    assertNull(streetNameManager.getValue("us", streetName));

    // random key
    String randomStreetName = streetNameManager.getRandomKey();
    assertNotNull(randomStreetName);
    assertFalse(randomStreetName.isEmpty());
  }
}
