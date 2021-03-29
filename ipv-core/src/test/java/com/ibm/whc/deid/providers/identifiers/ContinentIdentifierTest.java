/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class ContinentIdentifierTest implements MaskingProviderTest {
  @Test
  public void testIsOfThisType() throws Exception {
    ContinentIdentifier identifier = new ContinentIdentifier(tenantId, localizationProperty);

    String[] validContinents = {"Asia", "Europe", "Africa", "europe"};

    for (String continent : validContinents) {
      assertTrue(identifier.isOfThisType(continent));
    }

    String[] invalidContinents = {"12344", "Eurape"};

    for (String continent : invalidContinents) {
      assertFalse(identifier.isOfThisType(continent));
    }
  }
}
