/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class HospitalIdentifierTest implements MaskingProviderTest {
  @Test
  public void testIdentifier() {
    Identifier identifier = new HospitalIdentifier(tenantId, localizationProperty);

    String hospitalName = "York Hospital";
    assertTrue(identifier.isOfThisType(hospitalName));

    hospitalName = "york hospital";
    assertTrue(identifier.isOfThisType(hospitalName));
  }


}
