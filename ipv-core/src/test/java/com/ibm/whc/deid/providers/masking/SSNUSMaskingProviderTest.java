/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.models.SSNUS;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.SSNUSIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.SSNUSMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;

public class SSNUSMaskingProviderTest extends TestLogSetUp {

  /*
   * Tests for both preserve area number and group options and their boolean values (true and
   * false). Also tests for an invalid value.
   */

  @Test
  public void testMaskPreserveAreaAndGroup() {
    // The preserve area number and group options default values are set to
    // TRUE.
    MaskingProvider maskingProvider = new SSNUSMaskingProvider();
    SSNUSIdentifier identifier = new SSNUSIdentifier();

    String ssn = "123-54-9876";
    boolean changed = false;
    for (int i = 0; i < 10; i++) {
      String maskedValue = maskingProvider.mask(ssn);
      if (!ssn.equals(maskedValue)) {
        changed = true;
      }
      assertTrue(maskedValue, identifier.isOfThisType(maskedValue));
      assertTrue(maskedValue.startsWith("123-54-"));
    }
    assertTrue(changed);
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
    SSNUS ssnUS = identifier.parseSSNUS(ssn);
    String ssnGroup = ssnUS.getGroup();
    boolean changed = false;
    for (int i = 0; i < 10; i++) {
      String maskedValue = maskingProvider.mask(ssn);
      if (!ssn.equals(maskedValue)) {
        changed = true;
      }
      assertTrue(identifier.isOfThisType(maskedValue));
      assertEquals(ssnGroup, identifier.parseSSNUS(maskedValue).getGroup());
    }
    assertTrue(changed);
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
    for (int i = 0; i < 10; i++) {
      String maskedValue = maskingProvider.mask(ssnValue);
      assertTrue(identifier.isOfThisType(maskedValue));

      SSNUS ssn = identifier.parseSSNUS(maskedValue);

      if (!"123".equals(ssn.getAreaNumber())) {
        randomizationOKArea++;
      }
      if (!"12".equals(ssn.getGroup())) {
        randomizationOKGroup++;
      }
    }
    assertTrue(randomizationOKGroup > 0);
    assertTrue(randomizationOKArea > 0);

    // since not preserving, input need not be valid

    ssnValue = "invalid";
    for (int i = 0; i < 10; i++) {
      String maskedValue = maskingProvider.mask(ssnValue);
      assertTrue(identifier.isOfThisType(maskedValue));
    }
  }

  @Test
  public void testMaskNullSSNUSInputReturnNull() throws Exception {
    SSNUSMaskingProviderConfig configuration = new SSNUSMaskingProviderConfig();
    MaskingProvider maskingProvider = new SSNUSMaskingProvider(configuration);

    String invalidSSNUS = null;
    String maskedSSNUS = maskingProvider.mask(invalidSSNUS);

    assertNull(maskedSSNUS);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUSInputValidHandlingReturnNull() throws Exception {
    SSNUSMaskingProviderConfig configuration = new SSNUSMaskingProviderConfig();
    configuration.setMaskPreserveAreaNumber(false);
    configuration.setMaskPreserveGroup(true);
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    MaskingProvider maskingProvider = new SSNUSMaskingProvider(configuration);

    String invalidSSNUS = "Invalid SSNUS";
    String maskedSSNUS = maskingProvider.mask(invalidSSNUS);

    assertNull(maskedSSNUS);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUSInputValidHandlingReturnRandom() throws Exception {
    SSNUSMaskingProviderConfig configuration = new SSNUSMaskingProviderConfig();
    configuration.setMaskPreserveAreaNumber(true);
    configuration.setMaskPreserveGroup(false);
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
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
    configuration.setMaskPreserveAreaNumber(true);
    configuration.setMaskPreserveGroup(true);
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    MaskingProvider maskingProvider = new SSNUSMaskingProvider(configuration);

    String invalidSSNUS = "Invalid SSNUS";
    String maskedSSNUS = maskingProvider.mask(invalidSSNUS);

    assertEquals("OTHER", maskedSSNUS);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUSInputValidHandlingReturnNonDefaultCustomValue() throws Exception {
    SSNUSMaskingProviderConfig configuration = new SSNUSMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    // default preserve true
    configuration.setUnexpectedInputReturnMessage("Test SSNUS");
    MaskingProvider maskingProvider = new SSNUSMaskingProvider(configuration);

    String invalidSSNUS = "Invalid SSNUS";
    String maskedSSNUS = maskingProvider.mask(invalidSSNUS);

    assertEquals("Test SSNUS", maskedSSNUS);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidSSNUSInputInvalidHandlingExit() throws Exception {
    SSNUSMaskingProviderConfig configuration = new SSNUSMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.ERROR_EXIT);
    MaskingProvider maskingProvider = new SSNUSMaskingProvider(configuration);

    String invalidSSNUS = "Invalid SSNUS";
    try {
      maskingProvider.mask(invalidSSNUS);
      fail("expected exception");
    } catch (PrivacyProviderInvalidInputException e) {
      assertFalse(e.getMessage().contains(invalidSSNUS));
    }
  }
}
