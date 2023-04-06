/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class EmailIdentifierTest {
  @Test
  public void testIsOfThisType() throws Exception {
    EmailIdentifier identifier = new EmailIdentifier();

    assertTrue(identifier.isOfThisType("bob@bob.com"));
    assertFalse(identifier.isOfThisType("somethin else"));
  }
}
