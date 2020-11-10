/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class MACAddressIdentifierTest {
  @Test
  public void testIsOfThisType() throws Exception {
    MACAddressIdentifier identifier = new MACAddressIdentifier();

    assertTrue(identifier.isOfThisType("00:0a:95:9d:68:16"));
    assertTrue(identifier.isOfThisType("00:0A:95:9D:68:16"));
    assertFalse(identifier.isOfThisType("somethin else"));
  }
}
