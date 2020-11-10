/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class CreditCardTypeIdentifierTest {
  @Test
  public void testIsOfThisType() {
    CreditCardTypeIdentifier identifier = new CreditCardTypeIdentifier();

    String originalValue = "VISA";
    assertTrue(identifier.isOfThisType(originalValue));
    originalValue = "vISa";
    assertTrue(identifier.isOfThisType(originalValue));
  }
}
