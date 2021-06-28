/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class CreditCardTypeIdentifierTest implements MaskingProviderTest {
  @Test
  public void testIsOfThisType() {
    CreditCardTypeIdentifier identifier =
        new CreditCardTypeIdentifier(tenantId, localizationProperty);

    assertTrue(identifier.isOfThisType("VISA"));
    assertTrue(identifier.isOfThisType("vISa"));
    assertTrue(identifier.isOfThisType("mastercard"));
    assertTrue(identifier.isOfThisType("MasterCard"));
    assertFalse(identifier.isOfThisType("MasterCharge"));
    assertFalse(identifier.isOfThisType(""));
    assertFalse(identifier.isOfThisType(null));
    assertTrue(identifier.isOfThisType("American Express"));
    assertFalse(identifier.isOfThisType("AmericanExpress"));
    assertTrue(identifier.isOfThisType("AMEX"));
    assertTrue(identifier.isOfThisType("amex"));
    assertTrue(identifier.isOfThisType("Discover"));
    assertTrue(identifier.isOfThisType("JCB"));
    assertFalse(identifier.isOfThisType("BankAmericard"));
    assertTrue(identifier.isOfThisType("Diners Club"));
    assertFalse(identifier.isOfThisType("DinersClub"));
    assertFalse(identifier.isOfThisType("Diner'sClub"));
  }
}
