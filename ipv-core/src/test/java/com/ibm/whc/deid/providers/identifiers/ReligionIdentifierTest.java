/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class ReligionIdentifierTest {
  @Test
  public void testIsOfThisType() throws Exception {
    ReligionIdentifier religionIdentifier = new ReligionIdentifier();
    String religion = "Buddhist";
    assertTrue(religionIdentifier.isOfThisType(religion));
  }
}
