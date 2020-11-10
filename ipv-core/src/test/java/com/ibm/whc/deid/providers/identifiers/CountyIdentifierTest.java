/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class CountyIdentifierTest {
  @Test
  public void testFullName() {
    Identifier identifier = new CountyIdentifier();

    String originalValue = "Pendleton County";
    assertTrue(identifier.isOfThisType(originalValue));
  }

  @Test
  public void testShortName() {
    Identifier identifier = new CountyIdentifier();

    String originalValue = "Pendleton";
    assertTrue(identifier.isOfThisType(originalValue));
  }
}
