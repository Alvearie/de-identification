/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class ICDv9IdentifierTest implements MaskingProviderTest {

  @Test
  public void testIsOfThisType() throws Exception {
    ICDv9Identifier identifier = new ICDv9Identifier(tenantId, localizationProperty);

    String icdCode = "004.8";
    assertTrue(identifier.isOfThisType(icdCode));

    String icdShortName = "Staph Food Poisoning";
    assertTrue(identifier.isOfThisType(icdShortName));

    String icdShortNameLower = "Staph Food Poisoning".toLowerCase();
    assertTrue(identifier.isOfThisType(icdShortNameLower));

    String icdFullName = "Staphylococcal Food Poisoning";
    assertTrue(identifier.isOfThisType(icdFullName));

    assertFalse(identifier.isOfThisType("XXXX"));
  }
}
