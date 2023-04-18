/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class CreditCardIdentifierTest {
  private CreditCardIdentifier identifier;

  @Before
  public void setUp() throws Exception {
    identifier = new CreditCardIdentifier();
  }

  @Test
  public void testAMEX() throws Exception {
    assertTrue(identifier.isOfThisType("370000992821860"));
  }

  @Test
  public void testDC() throws Exception {
    assertTrue(identifier.isOfThisType("30000099611752"));
  }

  @Test
  public void testDISC() throws Exception {
    assertTrue(identifier.isOfThisType("6011009285752817"));
  }

  @Ignore
  @Test
  public void testJBC() throws Exception {
    assertTrue(identifier.isOfThisType("3088009773563374"));
  }

  @Test
  public void testMC() throws Exception {
    assertTrue(identifier.isOfThisType("5500009337062017"));
  }

  @Test
  public void testVisa() throws Exception {
    assertTrue(identifier.isOfThisType("4111119762378756")); // VISA
  }

  @Test
  public void testError() throws Exception {
    assertFalse(identifier.isOfThisType("fjadlsjfal;sf"));
    assertFalse(identifier.isOfThisType("1234567890"));
  }
}
