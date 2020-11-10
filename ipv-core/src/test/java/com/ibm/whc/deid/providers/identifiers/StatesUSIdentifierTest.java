/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class StatesUSIdentifierTest {
  @Test
  public void testIdentification() {
    StatesUSIdentifier identifier = new StatesUSIdentifier();
    String value = "Alabama";
    assertTrue(identifier.isOfThisType(value));

    value = "AL";
    assertTrue(identifier.isOfThisType(value));
  }

  @Test
  public void testIdentificationIgnoresCase() {
    StatesUSIdentifier identifier = new StatesUSIdentifier();
    String value = "aLABama";
    assertTrue(identifier.isOfThisType(value));
  }
}
