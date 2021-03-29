/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class GenderIdentifierTest implements MaskingProviderTest {
  @Test
  public void testMatch() {
    Identifier identifier = new GenderIdentifier(tenantId, localizationProperty);

    String value = "Male";
    assertTrue(identifier.isOfThisType(value));
  }

  @Test
  public void testMatchIgnoresCase() {
    Identifier identifier = new GenderIdentifier(tenantId, localizationProperty);

    String value = "MaLE";
    assertTrue(identifier.isOfThisType(value));
  }
}
