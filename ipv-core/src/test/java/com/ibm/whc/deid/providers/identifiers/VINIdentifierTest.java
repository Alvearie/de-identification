/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class VINIdentifierTest {
  @Test
  public void testIsOfThisType() {
    VINIdentifier vinIdentifier = new VINIdentifier();

    /* VIN value too short */
    String vin = "ABV3231333";
    assertFalse(vinIdentifier.isOfThisType(vin));

    /* VIN contains invalid characters */
    vin = "ABQ12345678901234"; // char Q is invalid
    assertFalse(vinIdentifier.isOfThisType(vin));
    vin = "AIB12345678901234"; // char I is invalid
    assertFalse(vinIdentifier.isOfThisType(vin));
    vin = "AOB12345678901234"; // char O is invalid
    assertFalse(vinIdentifier.isOfThisType(vin));

    /* VIN contains non-digits or non-letters */
    vin = "1B312-45678901234"; // char O is invalid
    assertFalse(vinIdentifier.isOfThisType(vin));
    vin = "1B312.45678901234"; // char O is invalid
    assertFalse(vinIdentifier.isOfThisType(vin));

    /* VIN contains a not-known WMI 111 */
    vin = "11112345678901234";
    assertFalse(vinIdentifier.isOfThisType(vin));

    vin = "1B312345678901234"; // char O is invalid
    assertTrue(vinIdentifier.isOfThisType(vin));
  }
}
