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

import com.ibm.whc.deid.models.SSNUS;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.SSNUSIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.SSNUSMaskingProviderConfig;
import org.junit.Test;

public class SSNUSMaskingProviderTest extends TestLogSetUp {
  // @formatter:off
  /*
   * Tests for both preserve area number and group options and their boolean
   * values (true and false). Also tests for an invalid value.
   */
  @Test
  public void testMaskPreserveAreaAndGroup() {

    // The preserve area number and group options default values are set to
    // TRUE.
    MaskingProvider maskingProvider = new SSNUSMaskingProvider();
    SSNUSIdentifier identifier = new SSNUSIdentifier();

    String ssn = "123-54-9876";
    String maskedValue = maskingProvider.mask(ssn);

    assertFalse(maskedValue.equals(ssn));
    assertTrue(identifier.isOfThisType(maskedValue));
    assertTrue(maskedValue.startsWith("123-54-"));
  }

  @Test
  public void testMaskPreserveGroup() {

    // The preserve area number and group options default values are set to
    // TRUE.
    SSNUSMaskingProviderConfig configuration = new SSNUSMaskingProviderConfig();
    configuration.setMaskPreserveAreaNumber(false);
    MaskingProvider maskingProvider = new SSNUSMaskingProvider(configuration);
    SSNUSIdentifier identifier = new SSNUSIdentifier();

    String ssn = "123-54-9876";
    String maskedValue = maskingProvider.mask(ssn);
    SSNUS ssnUS = identifier.parseSSNUS(ssn);
    SSNUS maskedUS = identifier.parseSSNUS(maskedValue);

    assertFalse(maskedValue.equals(ssn));
    assertTrue(identifier.isOfThisType(maskedValue));
    assertFalse("Masked value:" + maskedValue, maskedValue.startsWith("123"));
    assertTrue(ssnUS.getGroup().equals(maskedUS.getGroup()));
  }

  @Test
  public void testMaskNoPreserving() {
    SSNUSMaskingProviderConfig configuration = new SSNUSMaskingProviderConfig();
    configuration.setMaskPreserveAreaNumber(false);
    configuration.setMaskPreserveGroup(false);
    MaskingProvider maskingProvider = new SSNUSMaskingProvider(configuration);
    SSNUSIdentifier identifier = new SSNUSIdentifier();

    String ssnValue = "123-12-1234";

    int randomizationOKGroup = 0;
    int randomizationOKArea = 0;

    for (int i = 0; i < 1000; i++) {
      String maskedValue = maskingProvider.mask(ssnValue);
      assertFalse(maskedValue.equals(ssnValue));

      assertTrue(identifier.isOfThisType(maskedValue));
      SSNUS ssn = identifier.parseSSNUS(maskedValue);

      if (!(ssn.getAreaNumber().equals("123"))) {
        randomizationOKArea++;
      }

      if (!(ssn.getGroup().equals("12"))) {
        randomizationOKGroup++;
      }
    }

    assertTrue(randomizationOKGroup > 0);
    assertTrue(randomizationOKArea > 0);
  }

  @Test
  public void testMaskNullSSNUSInputReturnNull() throws Exception {
    SSNUSMaskingProviderConfig configuration = new SSNUSMaskingProviderConfig();
    MaskingProvider maskingProvider = new SSNUSMaskingProvider(configuration);

    String invalidSSNUS = null;
    String maskedSSNUS = maskingProvider.mask(invalidSSNUS);

    assertEquals(null, maskedSSNUS);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUSInputValidHandlingReturnNull() throws Exception {
    SSNUSMaskingProviderConfig configuration = new SSNUSMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider = new SSNUSMaskingProvider(configuration);

    String invalidSSNUS = "Invalid SSNUS";
    String maskedSSNUS = maskingProvider.mask(invalidSSNUS);

    assertEquals(null, maskedSSNUS);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUSInputValidHandlingReturnRandom() throws Exception {
    SSNUSMaskingProviderConfig configuration = new SSNUSMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider = new SSNUSMaskingProvider(configuration);
    Identifier identifier = new SSNUSIdentifier();

    String invalidSSNUS = "Invalid SSNUS";
    String maskedSSNUS = maskingProvider.mask(invalidSSNUS);

    assertFalse(maskedSSNUS.equals(invalidSSNUS));
    assertTrue(identifier.isOfThisType(maskedSSNUS));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUSInputValidHandlingReturnDefaultCustomValue() throws Exception {
    SSNUSMaskingProviderConfig configuration = new SSNUSMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new SSNUSMaskingProvider(configuration);

    String invalidSSNUS = "Invalid SSNUS";
    String maskedSSNUS = maskingProvider.mask(invalidSSNUS);

    assertEquals("OTHER", maskedSSNUS);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUSInputValidHandlingReturnNonDefaultCustomValue() throws Exception {
    SSNUSMaskingProviderConfig configuration = new SSNUSMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("Test SSNUS");
    MaskingProvider maskingProvider = new SSNUSMaskingProvider(configuration);

    String invalidSSNUS = "Invalid SSNUS";
    String maskedSSNUS = maskingProvider.mask(invalidSSNUS);

    assertEquals("Test SSNUS", maskedSSNUS);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUSInputInvalidHandlingReturnNull() throws Exception {
    SSNUSMaskingProviderConfig configuration = new SSNUSMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new SSNUSMaskingProvider(configuration);

    String invalidSSNUS = "Invalid SSNUS";
    String maskedSSNUS = maskingProvider.mask(invalidSSNUS);

    assertEquals(null, maskedSSNUS);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }
}
