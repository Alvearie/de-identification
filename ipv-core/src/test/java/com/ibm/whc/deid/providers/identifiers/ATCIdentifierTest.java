/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class ATCIdentifierTest {
  @Test
  public void testIsOfThisType() {
    Identifier identifier = new ATCIdentifier();

    String atc = "A04AA02";
    assertTrue(identifier.isOfThisType(atc));
    atc = "a02aa01";
    assertTrue(identifier.isOfThisType(atc));
  }
}
