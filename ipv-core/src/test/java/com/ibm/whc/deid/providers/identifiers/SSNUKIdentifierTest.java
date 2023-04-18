/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class SSNUKIdentifierTest {
  @Test
  public void testIsOfThisType() {
    SSNUKIdentifier identifier = new SSNUKIdentifier();

    String ssn = "AB123456C";
    assertTrue(identifier.isOfThisType(ssn));

    // ignores spaces
    ssn = "AB 12 34 56 C";
    assertTrue(identifier.isOfThisType(ssn));

    // check for not allowed characters
    ssn = "DB123456C";
    assertFalse(identifier.isOfThisType(ssn));
    ssn = "AD123456C";
    assertFalse(identifier.isOfThisType(ssn));
    ssn = "AO123456C";
    assertFalse(identifier.isOfThisType(ssn));
    ssn = "BA12A456C";
    assertFalse(identifier.isOfThisType(ssn));
    ssn = "BA1234567";
    assertFalse(identifier.isOfThisType(ssn));
    ssn = "BA123456Z";
    assertFalse(identifier.isOfThisType(ssn));

    // 'O' is allowed on the first character
    ssn = "OA123456C";
    assertTrue(identifier.isOfThisType(ssn));
  }
}
