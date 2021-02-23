/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.models.PhoneNumber;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class PhoneIdentifierTest {
	private String tenantId = "TEST_TENANT";
	private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

  @Test
  public void testIsOfThisType() throws Exception {
    PhoneIdentifier identifier = new PhoneIdentifier(null, tenantId, localizationProperty);

    String[] validNumbers = {"+353-0876653255", "00353-0876653255", "+353-(087)6653255",
        "0044-(087)6653255", "0044 (087)6653255", "0044 0876653255",};
    for (String number : validNumbers) {
      assertTrue(identifier.isOfThisType(number));
    }

    String[] invalidNumbers = {"6653255", "+44", "+44-123444a122"};

    for (String number : invalidNumbers) {
      assertFalse(identifier.isOfThisType(number));
    }

    String[] validUSNumbers = {"3471234567", // New York
        "2601234555" // Indiana
    };

    for (String number : validUSNumbers) {
      assertTrue(identifier.isOfThisType(number));
    }

    String[] invalidUSNumbers = {"347123456", // it is not 10-digit
        "260123455a" // it contains letters
    };

    for (String number : invalidUSNumbers) {
      assertFalse(identifier.isOfThisType(number));
    }
  }

  @Test
  public void testGetPhone() throws Exception {
    PhoneIdentifier identifier = new PhoneIdentifier(null, tenantId, localizationProperty);
    String phoneNumber = "+353-0876653255";

    PhoneNumber number = identifier.getPhoneNumber(phoneNumber);
    assertTrue(number.getPrefix().equals("+"));
    assertTrue(number.getCountryCode().equals("353"));
    assertTrue(number.getNumber().equals("0876653255"));

    // US number with prefix
    phoneNumber = "+1-3471234567";

    number = identifier.getPhoneNumber(phoneNumber);
    assertTrue(number.getPrefix().equals("+"));
    assertTrue(number.getCountryCode().equals("1"));
    assertTrue(number.getAreaCode().equals("347"));
    assertTrue(number.getNumber().equals("1234567"));
  }
}
