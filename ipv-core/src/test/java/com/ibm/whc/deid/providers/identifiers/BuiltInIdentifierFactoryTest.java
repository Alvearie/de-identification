/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import org.junit.Test;
import com.ibm.whc.deid.identifiers.EntityType;

public class BuiltInIdentifierFactoryTest {

  @Test
  public void testGetIdentifier() {
    BuiltInIdentifierFactory identifierFactory = new BuiltInIdentifierFactory();
    String tenantId = "TEST_TENANT";
    for (EntityType type : EntityType.values()) {
      assertNotNull(identifierFactory.getIdentifier(type, tenantId));
    }
  }

  @Test
  public void testGetAvailableIdentifiers() {
    // @formatter:off
    String[] clazzNames = new String[] { 
        "AddressIdentifier", 
        "ATCIdentifier",
        "CityIdentifier",
        "ContinentIdentifier",
        "CountryIdentifier",
        "CountyIdentifier",
        "CreditCardIdentifier",
        "CreditCardTypeIdentifier",
        "DateTimeIdentifier",
        "EmailIdentifier",
        "GenderIdentifier",
        "HospitalIdentifier",
        "IBANIdentifier",
        "ICDv9Identifier",
        "ICDv10Identifier",
        "IMEIIdentifier",
        "IPAddressIdentifier",
        "LatitudeLongitudeIdentifier",
        "MACAddressIdentifier",
        "MaritalStatusIdentifier",
        "NameIdentifier",
        "OccupationIdentifier",
        "RaceEthnicityIdentifier",
        "ReligionIdentifier",
        "StatesUSIdentifier",
        "SWIFTCodeIdentifier",
        "VINIdentifier",
        "ZIPCodeIdentifier"
    };
    // @formatter:on
    Set<String> clazzNamesSet = new HashSet<>(Arrays.asList(clazzNames));
    BuiltInIdentifierFactory identifierFactory = new BuiltInIdentifierFactory();
    String tenantId = "TEST_TENANT";
    Collection<Identifier> identifiers = identifierFactory.getAvailableIdentifiers(tenantId);
    assertTrue(clazzNamesSet.size() == identifiers.size());
    for (Identifier identifier : identifiers) {
      String className = identifier.getClass().getSimpleName();
      assertTrue("identifier "+ className + " not in class list",
          clazzNamesSet.contains(className));
    }
  }
}
