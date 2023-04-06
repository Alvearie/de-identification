/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.models.SSNUS;
import org.junit.Test;

public class SSNUSIdentifierTest {
  @Test
  public void testOfThisType() {
    SSNUSIdentifier identifier = new SSNUSIdentifier();

    String ssnValue = "123-12-1234";
    assertTrue(identifier.isOfThisType(ssnValue));
    ssnValue = "1234-12-1234";
    assertFalse(identifier.isOfThisType(ssnValue));
    ssnValue = "12a-12-1234";
    assertFalse(identifier.isOfThisType(ssnValue));
    ssnValue = "123-123-1234";
    assertFalse(identifier.isOfThisType(ssnValue));
    ssnValue = "123-12-12345";
    assertFalse(identifier.isOfThisType(ssnValue));
    ssnValue = "123-12-a234";
    assertFalse(identifier.isOfThisType(ssnValue));
    ssnValue = "123-1b-1234";
    assertFalse(identifier.isOfThisType(ssnValue));

    // invalid group number
    ssnValue = "000-12-1234";
    assertFalse(identifier.isOfThisType(ssnValue));
  }

  @Test
  public void testNoHyphens() {
    SSNUSIdentifier identifier = new SSNUSIdentifier();

    String ssnValue = "123121234";
    assertTrue(identifier.isOfThisType(ssnValue));
    ssnValue = "1234121234";
    assertFalse(identifier.isOfThisType(ssnValue));
    ssnValue = "12a121234";
    assertFalse(identifier.isOfThisType(ssnValue));
    ssnValue = "1231231234";
    assertFalse(identifier.isOfThisType(ssnValue));
    ssnValue = "1231212345";
    assertFalse(identifier.isOfThisType(ssnValue));
    ssnValue = "12312a234";
    assertFalse(identifier.isOfThisType(ssnValue));
    ssnValue = "1231b1234";
    assertFalse(identifier.isOfThisType(ssnValue));
    ssnValue = "000121234";
    assertFalse(identifier.isOfThisType(ssnValue));
  }

  @Test
  public void testParse() {
    SSNUSIdentifier identifier = new SSNUSIdentifier();

    String ssnValue = "123-12-1234";
    SSNUS ssn = identifier.parseSSNUS(ssnValue);
    assertTrue(ssn != null);
    assertEquals("123", ssn.getAreaNumber());;
    assertEquals("12", ssn.getGroup());
    assertEquals("1234", ssn.getSerialNumber());
  }
}
