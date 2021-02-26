/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class StatesUSIdentifierTest implements MaskingProviderTest {
  @Test
  public void testIdentification() {
    StatesUSIdentifier identifier = new StatesUSIdentifier(tenantId, localizationProperty);
    String value = "Alabama";
    assertTrue(identifier.isOfThisType(value));

    value = "AL";
    assertTrue(identifier.isOfThisType(value));
  }

  @Test
  public void testIdentificationIgnoresCase() {
    StatesUSIdentifier identifier = new StatesUSIdentifier(tenantId, localizationProperty);
    String value = "aLABama";
    assertTrue(identifier.isOfThisType(value));
  }
}
