/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class IMEIIdentifierTest {
  @Test
  public void testIsOfThisType() {
    IMEIIdentifier identifier = new IMEIIdentifier();

    String imei = "001013001234568"; // OK
    assertTrue(identifier.isOfThisType(imei));

    imei = "001013001234567"; // invalid check digit
    assertFalse(identifier.isOfThisType(imei));

    imei = "12312313"; // short
    assertFalse(identifier.isOfThisType(imei));

    imei = "001013001a34567"; // contains letters
    assertFalse(identifier.isOfThisType(imei));
  }
}
