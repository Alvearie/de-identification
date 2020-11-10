/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.SSNUKIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.SSNUKMaskingProviderConfig;
import org.junit.Test;

public class SSNUKMaskingProviderTest extends TestLogSetUp {
  // @formatter:off
  /*
   * Tests for prefix preservation option and its boolean values (true and
   * false). Also tests for an invalid value.
   */

  @Test
  public void testMaskPrefixPreservation() {
    MaskingProvider maskingProvider = new SSNUKMaskingProvider();
    SSNUKIdentifier identifier = new SSNUKIdentifier();

    String ssn = "AB123456C";
    String maskedValue = maskingProvider.mask(ssn);

    assertFalse(maskedValue.equals(ssn));
    assertTrue(identifier.isOfThisType(maskedValue));
    assertTrue(maskedValue.startsWith("AB"));
  }

  @Test
  public void testMaskNoPrefixPreservation() {
    SSNUKMaskingProviderConfig configuration = new SSNUKMaskingProviderConfig();
    configuration.setMaskPreservePrefix(false);
    MaskingProvider maskingProvider = new SSNUKMaskingProvider(configuration);
    SSNUKIdentifier identifier = new SSNUKIdentifier();

    String ssn = "AB123456C";

    int count = 0;

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(ssn);
      assertFalse(maskedValue.equals(ssn));
      assertTrue(identifier.isOfThisType(maskedValue));

      if (!maskedValue.startsWith("AB")) {
        count++;
      }
    }

    assertTrue(count > 0);
  }

  @Test
  public void testMaskNullSSNUKInputReturnNull() throws Exception {
    SSNUKMaskingProviderConfig configuration = new SSNUKMaskingProviderConfig();
    configuration.setMaskPreservePrefix(false);
    MaskingProvider maskingProvider = new SSNUKMaskingProvider(configuration);

    String invalidSSNUK = null;
    String maskedSSNUK = maskingProvider.mask(invalidSSNUK);

    assertEquals(null, maskedSSNUK);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUKInputValidHandlingReturnNull() throws Exception {
    SSNUKMaskingProviderConfig configuration = new SSNUKMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider = new SSNUKMaskingProvider(configuration);

    String invalidSSNUK = "Invalid SSNUK";
    String maskedSSNUK = maskingProvider.mask(invalidSSNUK);

    assertEquals(null, maskedSSNUK);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUKInputValidHandlingReturnRandom() throws Exception {
    SSNUKMaskingProviderConfig configuration = new SSNUKMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider = new SSNUKMaskingProvider(configuration);

    Identifier identifier = new SSNUKIdentifier();

    String invalidSSNUK = "Invalid SSNUK";
    String maskedSSNUK = maskingProvider.mask(invalidSSNUK);

    assertFalse(maskedSSNUK.equals(invalidSSNUK));
    assertTrue(identifier.isOfThisType(maskedSSNUK));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUKInputValidHandlingReturnDefaultCustomValue() throws Exception {
    SSNUKMaskingProviderConfig configuration = new SSNUKMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new SSNUKMaskingProvider(configuration);

    String invalidSSNUK = "Invalid SSNUK";
    String maskedSSNUK = maskingProvider.mask(invalidSSNUK);

    assertEquals("OTHER", maskedSSNUK);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUKInputValidHandlingReturnNonDefaultCustomValue() throws Exception {
    SSNUKMaskingProviderConfig configuration = new SSNUKMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("Test SSNUK");
    MaskingProvider maskingProvider = new SSNUKMaskingProvider(configuration);

    String invalidSSNUK = "Invalid SSNUK";
    String maskedSSNUK = maskingProvider.mask(invalidSSNUK);

    assertEquals("Test SSNUK", maskedSSNUK);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUKInputInvalidHandlingReturnNull() throws Exception {
    SSNUKMaskingProviderConfig configuration = new SSNUKMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new SSNUKMaskingProvider(configuration);

    String invalidSSNUK = "Invalid SSNUK";
    String maskedSSNUK = maskingProvider.mask(invalidSSNUK);

    assertEquals(null, maskedSSNUK);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }
}
