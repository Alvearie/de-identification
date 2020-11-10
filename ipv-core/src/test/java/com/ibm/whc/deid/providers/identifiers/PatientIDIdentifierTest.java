/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class PatientIDIdentifierTest {
  @Test
  public void testIsOfThisType() throws Exception {
    PatientIDIdentifier identifier = new PatientIDIdentifier();

    String[] validIDs = {"553-455-222-566"};
    for (String id : validIDs) {
      assertTrue(identifier.isOfThisType(id));
    }

    String[] invalidIDs = {"55A-455-222-566", "555-444-333"};
    for (String id : invalidIDs) {
      assertFalse(identifier.isOfThisType(id));
    }
  }
}
