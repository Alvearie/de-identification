/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class CountyIdentifierTest implements MaskingProviderTest {
  @Test
  public void testFullName() {
    Identifier identifier = new CountyIdentifier(tenantId, localizationProperty);

    String originalValue = "Pendleton County";
    assertTrue(identifier.isOfThisType(originalValue));
  }

  @Test
  public void testShortName() {
    Identifier identifier = new CountyIdentifier(tenantId, localizationProperty);

    String originalValue = "Pendleton";
    assertTrue(identifier.isOfThisType(originalValue));
  }
}
