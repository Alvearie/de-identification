/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class ReligionIdentifierTest implements MaskingProviderTest {
  @Test
  public void testIsOfThisType() throws Exception {
    ReligionIdentifier religionIdentifier = new ReligionIdentifier(tenantId, localizationProperty);
    String religion = "Buddhist";
    assertTrue(religionIdentifier.isOfThisType(religion));
  }
}
