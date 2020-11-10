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

import com.ibm.whc.deid.providers.identifiers.IBANIdentifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.shared.pojo.config.masking.IBANMaskingProviderConfig;
import org.junit.Test;

public class IBANMaskingProviderTest extends TestLogSetUp {
  /*
   * Tests preserve country option and its boolean values (true and false). Also tests for an
   * invalid value
   */

  @Test
  public void testMask() {
    // The preserve country option by default is set to true.
    IBANMaskingProvider maskingProvider = new IBANMaskingProvider();
    IBANIdentifier identifier = new IBANIdentifier();

    String iban = "IE71WZXH31864186813343";
    String maskedValue = maskingProvider.mask(iban);

    assertFalse(maskedValue.equals(iban));
    assertTrue(identifier.isOfThisType(maskedValue));
    // by default we preserve the country
    assertTrue(maskedValue.startsWith("IE"));
  }

  @Test
  public void testMaskNoCountryPreservation() {
    IBANMaskingProviderConfig configuration = new IBANMaskingProviderConfig();
    configuration.setMaskPreserveCountry(false);
    IBANMaskingProvider maskingProvider = new IBANMaskingProvider(configuration);
    IBANIdentifier identifier = new IBANIdentifier();

    String iban = "IE71WZXH31864186813343";

    int randomizationOK = 0;

    for (int i = 0; i < 500; i++) {
      String maskedValue = maskingProvider.mask(iban);
      assertFalse(maskedValue.equals(iban));
      assertTrue(identifier.isOfThisType(maskedValue));
      if (!maskedValue.startsWith("IE")) {
        randomizationOK++;
      }
    }

    assertTrue(randomizationOK > 0);
  }

  @Test
  public void testMaskNullIBANInputReturnNull() throws Exception {
    IBANMaskingProviderConfig configuration = new IBANMaskingProviderConfig();
    MaskingProvider maskingProvider = new IBANMaskingProvider(configuration);

    String invalidIBAN = null;
    String maskedIBAN = maskingProvider.mask(invalidIBAN);

    assertEquals(null, maskedIBAN);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidIBANInputValidHandlingReturnNull() throws Exception {
    IBANMaskingProviderConfig configuration = new IBANMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider = new IBANMaskingProvider(configuration);

    String invalidIBAN = "Invalid IBAN";
    String maskedIBAN = maskingProvider.mask(invalidIBAN);

    assertEquals(null, maskedIBAN);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidIBANInputValidHandlingReturnRandom() throws Exception {
    IBANMaskingProviderConfig configuration = new IBANMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider = new IBANMaskingProvider(configuration);
    Identifier identifier = new IBANIdentifier();

    String invalidIBAN = "Invalid IBAN";
    String maskedIBAN = maskingProvider.mask(invalidIBAN);

    assertFalse(maskedIBAN.equals(invalidIBAN));
    assertTrue(identifier.isOfThisType(maskedIBAN));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidIBANInputValidHandlingReturnDefaultCustomValue() throws Exception {
    IBANMaskingProviderConfig configuration = new IBANMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new IBANMaskingProvider(configuration);

    String invalidIBAN = "Invalid IBAN";
    String maskedIBAN = maskingProvider.mask(invalidIBAN);

    assertEquals("OTHER", maskedIBAN);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidIBANInputValidHandlingReturnNonDefaultCustomValue() throws Exception {
    IBANMaskingProviderConfig configuration = new IBANMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("Test IBAN");
    MaskingProvider maskingProvider = new IBANMaskingProvider(configuration);

    String invalidIBAN = "Invalid IBAN";
    String maskedIBAN = maskingProvider.mask(invalidIBAN);

    assertEquals("Test IBAN", maskedIBAN);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidIBANInputInvalidHandlingReturnNull() throws Exception {
    IBANMaskingProviderConfig configuration = new IBANMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new IBANMaskingProvider(configuration);

    String invalidIBAN = "Invalid IBAN";
    String maskedIBAN = maskingProvider.mask(invalidIBAN);

    assertEquals(null, maskedIBAN);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }
}
