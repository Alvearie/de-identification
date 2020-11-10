/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class RaceEthnicityIdentifierTest {
  @Test
  public void testIsOfThisType() throws Exception {
    RaceEthnicityIdentifier identifier = new RaceEthnicityIdentifier();

    String race = "White";
    assertTrue(identifier.isOfThisType(race));

    // ignores case
    assertTrue(identifier.isOfThisType("white"));
  }
}
