/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class CountryIdentifierTest implements MaskingProviderTest {
  /*
   * NOTE: this class is intentionally left brief since CountryIdentifier is a wrapper to
   * CountryManager. Please check CountryManagerTest.
   */
  @Test
  public void testIsOfThisType() {
    Identifier identifier = new CountryIdentifier(tenantId, localizationProperty);
    String country = "United States of America";
    assertTrue(identifier.isOfThisType(country));
  }
}
