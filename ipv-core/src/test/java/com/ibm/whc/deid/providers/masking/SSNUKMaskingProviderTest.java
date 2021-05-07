/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.SSNUKIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.SSNUKMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;

public class SSNUKMaskingProviderTest extends TestLogSetUp {

  /*
   * Tests for prefix preservation option and its boolean values (true and
   * false). Also tests for an invalid value.
   */

  @Test
  public void testMaskPrefixPreservation() {
    MaskingProvider maskingProvider = new SSNUKMaskingProvider();
    SSNUKIdentifier identifier = new SSNUKIdentifier();

    String ssn = "AB123456C";
    boolean changed = false;
    for (int i = 0; i < 20; i++) {
      String maskedValue = maskingProvider.mask(ssn);
      if (!ssn.equalsIgnoreCase(maskedValue)) {
        changed = true;
      }
      assertTrue(identifier.isOfThisType(maskedValue));
      assertTrue(maskedValue.startsWith("AB"));
    }
    assertTrue(changed);
  }

  @Test
  public void testMaskNoPrefixPreservation() {
    SSNUKMaskingProviderConfig configuration = new SSNUKMaskingProviderConfig();
    configuration.setMaskPreservePrefix(false);
    MaskingProvider maskingProvider = new SSNUKMaskingProvider(configuration);
    SSNUKIdentifier identifier = new SSNUKIdentifier();

    String ssn = "AB123456C";
    String prefix = ssn.substring(0, 2);
    boolean changed = false;
    boolean changedPrefix = false;
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(ssn);
      assertTrue(identifier.isOfThisType(maskedValue));
      if (!ssn.equalsIgnoreCase(maskedValue)) {
        changed = true;
      }
      if (!prefix.equalsIgnoreCase(maskedValue.substring(0, 2))) {
        changedPrefix = true;
      }
    }
    assertTrue(changed);
    assertTrue(changedPrefix);

    // since not preserving prefix, input need not be recognized
    ssn = "X";
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(ssn);
      assertTrue(identifier.isOfThisType(maskedValue));
    }
  }

  @Test
  public void testMaskNullSSNUKInputReturnNull() throws Exception {
    SSNUKMaskingProviderConfig configuration = new SSNUKMaskingProviderConfig();
    MaskingProvider maskingProvider = new SSNUKMaskingProvider(configuration);

    String invalidSSNUK = null;
    String maskedSSNUK = maskingProvider.mask(invalidSSNUK);

    assertEquals(null, maskedSSNUK);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUKInputValidHandlingReturnNull() throws Exception {
    SSNUKMaskingProviderConfig configuration = new SSNUKMaskingProviderConfig();
    configuration.setMaskPreservePrefix(true);
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider = new SSNUKMaskingProvider(configuration);

    String invalidSSNUK = "Invalid SSNUK";
    String maskedSSNUK = maskingProvider.mask(invalidSSNUK);

    assertNull(maskedSSNUK);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUKInputValidHandlingReturnRandom() throws Exception {
    SSNUKMaskingProviderConfig configuration = new SSNUKMaskingProviderConfig();
    configuration.setMaskPreservePrefix(true);
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    MaskingProvider maskingProvider = new SSNUKMaskingProvider(configuration);

    Identifier identifier = new SSNUKIdentifier();

    String invalidSSNUK = "Invalid SSNUK";
    for (int i = 0; i < 100; i++) {
      String maskedSSNUK = maskingProvider.mask(invalidSSNUK);
      assertTrue(identifier.isOfThisType(maskedSSNUK));
      assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
    }
  }

  @Test
  public void testMaskInvalidSSNUKInputValidHandlingReturnDefaultCustomValue() throws Exception {
    SSNUKMaskingProviderConfig configuration = new SSNUKMaskingProviderConfig();
    configuration.setMaskPreservePrefix(true);
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
    configuration.setMaskPreservePrefix(true);
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    configuration.setUnexpectedInputReturnMessage("Test SSNUK");
    configuration.setUnspecifiedValueReturnMessage("Test X SSNUK");
    MaskingProvider maskingProvider = new SSNUKMaskingProvider(configuration);

    String invalidSSNUK = "Invalid SSNUK";
    String maskedSSNUK = maskingProvider.mask(invalidSSNUK);

    assertEquals("Test SSNUK", maskedSSNUK);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUKInputInvalidHandlingReturnNull() throws Exception {
    SSNUKMaskingProviderConfig configuration = new SSNUKMaskingProviderConfig();
    configuration.setMaskPreservePrefix(true);
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new SSNUKMaskingProvider(configuration);

    String invalidSSNUK = "Invalid SSNUK";
    String maskedSSNUK = maskingProvider.mask(invalidSSNUK);

    assertNull(maskedSSNUK);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }
}
