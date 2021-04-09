/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class MSISDNManagerTest implements MaskingProviderTest {

  @Test
  public void testIsValidCountryNumDigits() {
    MSISDNManager mgr = MSISDNManager.buildMSISDNManager(tenantId, localizationProperty);

    // single value
    assertTrue(mgr.isValidCountryNumDigits("1", 10));
    assertFalse(mgr.isValidCountryNumDigits("1", 9)); // or value
    assertTrue(mgr.isValidCountryNumDigits("963", 8));
    assertTrue(mgr.isValidCountryNumDigits("963", 9));
    assertFalse(mgr.isValidCountryNumDigits("963", 7));
    // range value
    assertFalse(mgr.isValidCountryNumDigits("886", 4));
    assertTrue(mgr.isValidCountryNumDigits("886", 5));
    assertTrue(mgr.isValidCountryNumDigits("886", 6));
    assertTrue(mgr.isValidCountryNumDigits("886", 7));
    assertTrue(mgr.isValidCountryNumDigits("886", 8));
    assertFalse(mgr.isValidCountryNumDigits("886", 9));
    assertFalse(mgr.isValidCountryNumDigits("886", 9));
    // unknown country
    assertTrue(mgr.isValidCountryNumDigits("888", 9));
    assertTrue(mgr.isValidCountryNumDigits("888", 999));
    assertFalse(mgr.isValidCountryNumDigits("888", 0));
    assertFalse(mgr.isValidCountryNumDigits("888", -2));
  }

  @Test
  public void testIsValidUSNumber() {
    MSISDNManager mgr = MSISDNManager.buildMSISDNManager(tenantId, localizationProperty);

    // wrong length
    assertFalse(mgr.isValidUSNumber("701945259"));
    // wrong characters
    assertFalse(mgr.isValidUSNumber("701945a594"));
    // OK
    assertTrue(mgr.isValidUSNumber("7019452594"));
    // bad area code
    assertFalse(mgr.isValidUSNumber("3339452594"));
  }

  @Test
  public void testGetRandomPhoneNumberDigitsByCountry() {
    MSISDNManager mgr = MSISDNManager.buildMSISDNManager(tenantId, localizationProperty);

    assertEquals(Integer.valueOf(10), mgr.getRandomPhoneNumberDigitsByCountry("1"));

    for (int i = 0; i < 10; i++) {
      Integer value = mgr.getRandomPhoneNumberDigitsByCountry("886");
      assertNotNull(value);
      int val = value.intValue();
      assertTrue(val == 5 || val == 6 || val == 7 || val == 8);
    }

    assertNull(mgr.getRandomPhoneNumberDigitsByCountry("888"));

    String value = mgr.getRandomCountryCode();
    assertNotNull(value);
  }

  @Test
  public void testIsValidCountryCode() {
    MSISDNManager mgr = MSISDNManager.buildMSISDNManager(tenantId, localizationProperty);

    assertTrue(mgr.isValidCountryCode("1"));
    assertTrue(mgr.isValidCountryCode("222"));
    assertFalse(mgr.isValidCountryCode("888"));
  }
}
