/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class MaritalStatusIdentifierTest implements MaskingProviderTest {
  @Test
  public void testIsOfThisType() throws Exception {
		MaritalStatusIdentifier maritalStatusIdentifier = new MaritalStatusIdentifier(tenantId, localizationProperty);
    String status = "Single";
    assertTrue(maritalStatusIdentifier.isOfThisType(status));
  }
}
