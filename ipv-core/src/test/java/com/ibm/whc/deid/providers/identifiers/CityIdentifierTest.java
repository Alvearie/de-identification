/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class CityIdentifierTest {
  @Test
  public void testIsOfThisType() throws Exception {
    CityIdentifier identifier = new CityIdentifier();

    String[] validCities = {"Athens", "athens", "Dublin", "New York City"};

    for (String city : validCities) {
      assertTrue(identifier.isOfThisType(city));
    }

    String[] invalidCities = {"12344", "Doblin"};

    for (String city : invalidCities) {
      assertFalse(identifier.isOfThisType(city));
    }
  }
}
