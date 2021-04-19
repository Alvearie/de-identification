/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.providers.masking.MaskingProviderTest;
import com.ibm.whc.deid.util.Manager;

public class StatesUSIdentifierTest implements MaskingProviderTest {

  @Test
  public void testIdentification() {
    StatesUSIdentifier identifier = new StatesUSIdentifier(tenantId, localizationProperty);
    String value = "Alabama";
    assertTrue(identifier.isOfThisType(value));

    Manager manager = identifier.getManager();

    value = "AL";
    assertTrue(identifier.isOfThisType(value));

    assertFalse(identifier.isOfThisType("xx"));
    
    assertSame(manager, identifier.getManager());
  }

  @Test
  public void testIdentificationIgnoresCase() {
    StatesUSIdentifier identifier = new StatesUSIdentifier(tenantId, localizationProperty);
    String value = "aLABama";
    assertTrue(identifier.isOfThisType(value));
  }
}
