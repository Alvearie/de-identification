/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import com.ibm.whc.deid.providers.masking.MaskingProviderTest;

public class ICDv10IdentifierTest implements MaskingProviderTest {

  @Test
  public void testIsOfThisType_empty() throws Exception {

    // No codes are loaded into the manager - use the regular expression to recognize codes

    ICDv10Identifier identifier = new ICDv10Identifier(tenantId, localizationProperty);

    assertFalse(identifier.isOfThisType("A"));
    assertFalse(identifier.isOfThisType("AB"));
    assertFalse(identifier.isOfThisType("A0"));
    assertFalse(identifier.isOfThisType("AB1.0"));
    assertFalse(identifier.isOfThisType("A0B.0"));
    assertTrue(identifier.isOfThisType("A01.0"));
    assertFalse(identifier.isOfThisType("A0BX0"));
    assertFalse(identifier.isOfThisType("A0B00"));
    assertTrue(identifier.isOfThisType("a01.0"));
    assertTrue(identifier.isOfThisType("A01.123G"));
    assertFalse(identifier.isOfThisType("A01.0123G"));
    assertFalse(identifier.isOfThisType("A01"));
    assertFalse(identifier.isOfThisType("A01.0A23G"));
    assertFalse(identifier.isOfThisType("A01.0A23G"));
    assertTrue(identifier.isOfThisType("A01.1"));
    assertTrue(identifier.isOfThisType("A01.11"));
    assertTrue(identifier.isOfThisType("A01.111"));
    assertTrue(identifier.isOfThisType("A01.11B"));
    assertFalse(identifier.isOfThisType("A01.B"));
    assertFalse(identifier.isOfThisType("A011B"));
    assertFalse(identifier.isOfThisType("A01.1111"));
    assertTrue(identifier.isOfThisType("A01.111A"));
    assertTrue(identifier.isOfThisType("A01.111a"));
    assertTrue(identifier.isOfThisType("A01.111Z"));
    assertFalse(identifier.isOfThisType(".111"));

    assertFalse(identifier.isOfThisType("Typhoid Fever"));
  }

  @Test
  public void testIsOfThisType_loaded() throws Exception {

    // Codes are loaded into the manager - use those and not the regular expression

    // TESTA00.0,Test v10 Name A,A00-A09,Category Name A,A10-A19,Chapter Name A
    // TESTB00.0,Test v10 Name B,B00-B09,Category Name B,B10-B19,Chapter Name B
    // TESTC00.0,Test v10 Name C,C00-C09,Category Name C,C10-D19,Chapter Name C
    // TESTD00.0,Test v10 Name D,D00-D09,Category Name D,D10-D19,Chapter Name D
    // TESTE00.0,Test v10 Name E,E00-E09,Category Name E,C10-D19,Chapter Name E

    ICDv10Identifier identifier = new ICDv10Identifier(tenantId, TEST_LOCALIZATION_PROPERTIES);

    assertFalse(identifier.isOfThisType("A01.0"));
    assertFalse(identifier.isOfThisType("Typhoid Fever"));

    assertTrue(identifier.isOfThisType("TESTB00.0"));
    assertTrue(identifier.isOfThisType("tESTc00.0"));
    assertTrue(identifier.isOfThisType("tESTe00.0"));
    assertTrue(identifier.isOfThisType("tESTa00.0"));
    assertTrue(identifier.isOfThisType("tESTd00.0"));
    assertFalse(identifier.isOfThisType("TESTF00.0"));

    assertTrue(identifier.isOfThisType("Test v10 Name A"));
    assertTrue(identifier.isOfThisType("Test v10 Name A".toUpperCase()));
    assertTrue(identifier.isOfThisType("Test v10 Name A".toLowerCase()));
    assertTrue(identifier.isOfThisType("Test v10 Name D"));
  }
}
