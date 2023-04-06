/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class IBANIdentifierTest {
  @Test
  public void testIsOfThisType() {
    IBANIdentifier identifier = new IBANIdentifier();

    String iban = "IE71WZXH31864186813343";
    assertTrue(identifier.isOfThisType(iban));

    iban = "GR98";
    assertFalse(identifier.isOfThisType(iban));
  }
}
