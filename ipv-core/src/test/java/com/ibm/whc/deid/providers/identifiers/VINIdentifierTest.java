/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class VINIdentifierTest implements MaskingProviderTest {
  @Test
  public void testIsOfThisType() {
    VINIdentifier vinIdentifier = new VINIdentifier(tenantId, localizationProperty);

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
    vin = "ABq12345678901234"; // char Q is invalid
    assertFalse(vinIdentifier.isOfThisType(vin));
    vin = "AiB12345678901234"; // char I is invalid
    assertFalse(vinIdentifier.isOfThisType(vin));
    vin = "AoB12345678901234"; // char O is invalid
    assertFalse(vinIdentifier.isOfThisType(vin));

    /* VIN contains non-digits or non-letters */
    vin = "1B312-45678901234"; // char O is invalid
    assertFalse(vinIdentifier.isOfThisType(vin));
    vin = "1B312.45678901234"; // char O is invalid
    assertFalse(vinIdentifier.isOfThisType(vin));

    /* VIN contains a not-known WMI 111 */
    vin = "11112345678901234";
    assertFalse(vinIdentifier.isOfThisType(vin));

    vin = "1B312345678901234";
    assertTrue(vinIdentifier.isOfThisType(vin));

    /* null */
    assertFalse(vinIdentifier.isOfThisType(null));
  }
}
