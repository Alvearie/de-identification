/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class ICDv10IdentifierTest implements MaskingProviderTest {
  @Test
  public void testIsOfThisType() throws Exception {
    ICDv10Identifier identifier = new ICDv10Identifier(tenantId, localizationProperty);

    String icdCode = "A01.0";
    assertTrue(identifier.isOfThisType(icdCode));

    String icdShortName = "Typhoid Fever";
    assertTrue(identifier.isOfThisType(icdShortName));

    String icdShortNameLower = "Typhoid Fever".toLowerCase();
    assertTrue(identifier.isOfThisType(icdShortNameLower));
  }
}
