/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class GenderIdentifierTest {
  @Test
  public void testMatch() {
    Identifier identifier = new GenderIdentifier();

    String value = "Male";
    assertTrue(identifier.isOfThisType(value));
  }

  @Test
  public void testMatchIgnoresCase() {
    Identifier identifier = new GenderIdentifier();

    String value = "MaLE";
    assertTrue(identifier.isOfThisType(value));
  }
}
