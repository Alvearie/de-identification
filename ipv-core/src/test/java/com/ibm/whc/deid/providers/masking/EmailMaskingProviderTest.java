/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.EmailIdentifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.shared.pojo.config.masking.EmailMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;

public class EmailMaskingProviderTest extends TestLogSetUp {
  /*
   * Tests for: (1) various email addresses with default preserveDomain (value = 1), (2) an invalid
   * email address (e.g., foobar), and (3) preserveDomain values of 0 , 2 (sub-domain), and 5 (long
   * domain). Note: When preserveDomain is 0, the tld is set randomly and may/may not be the same as
   * the original tld.
   */
  @Test
  public void testMask() throws Exception {
    EmailMaskingProvider maskingProvider = new EmailMaskingProvider();

    assertThat(maskingProvider.mask("dummyEmail@ie.ibm.com"), not("dummyEmail@ie.ibm.com"));
    assertThat(maskingProvider.mask("adsfasdfafs12341@fdlkjfsal.com"),
        not("adsfasdfafs12341@fdlkjfsal.com"));
    assertThat(maskingProvider.mask("a12399@fdsaf.eu"), not("a12399@fdsaf.eu"));
    assertTrue(maskingProvider.mask("dummyname@test.com").endsWith(".com"));
    assertTrue(maskingProvider.mask("dummyname@test.co.uk").endsWith(".co.uk"));
    assertTrue(maskingProvider.mask("a12399@fdsaf.eu").endsWith(".eu"));
  }

  @Test
  public void testMaskNullEmailInputReturnNull() throws Exception {
    EmailMaskingProviderConfig configuration = new EmailMaskingProviderConfig();
    MaskingProvider maskingProvider = new EmailMaskingProvider(configuration);

    String invalidEmail = null;
    String maskedEmail = maskingProvider.mask(invalidEmail);

    assertEquals(null, maskedEmail);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidEmailInputValidHandlingReturnNull() throws Exception {
    EmailMaskingProviderConfig configuration = new EmailMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider = new EmailMaskingProvider(configuration);

    String invalidEmail = "Invalid Email";
    String maskedEmail = maskingProvider.mask(invalidEmail);

    assertEquals(null, maskedEmail);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidEmailInputValidHandlingReturnRandom() throws Exception {
    EmailMaskingProviderConfig configuration = new EmailMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider = new EmailMaskingProvider(configuration);
    Identifier identifier = new EmailIdentifier();

    String invalidEmail = "Invalid Email";
    String maskedEmail = maskingProvider.mask(invalidEmail);

    assertFalse(maskedEmail.equals(invalidEmail));
    assertTrue(identifier.isOfThisType(maskedEmail));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidEmailInputValidHandlingReturnRandomNew() throws Exception {
    EmailMaskingProviderConfig configuration = new EmailMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new EmailMaskingProvider(configuration);
    Identifier identifier = new EmailIdentifier();

    String invalidEmail = "Invalid Email";
    String maskedEmail = maskingProvider.mask(invalidEmail);

    assertNotEquals(invalidEmail, maskedEmail);
    assertNotEquals("OTHER", maskedEmail);
    assertTrue(identifier.isOfThisType(maskedEmail));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidEmailInputValidHandlingReturnDefaultCustomValue() throws Exception {
    EmailMaskingProviderConfig configuration = new EmailMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new EmailMaskingProvider(configuration);

    String invalidEmail = "Invalid Email";
    String maskedEmail = maskingProvider.mask(invalidEmail);

    assertEquals("OTHER", maskedEmail);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidEmailInputValidHandlingReturnNonDefaultCustomValue() throws Exception {
    EmailMaskingProviderConfig configuration = new EmailMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("Test Email");
    MaskingProvider maskingProvider = new EmailMaskingProvider(configuration);

    String invalidEmail = "Invalid Email";
    String maskedEmail = maskingProvider.mask(invalidEmail);

    assertEquals("Test Email", maskedEmail);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidEmailInputInvalidHandlingReturnNull() throws Exception {
    EmailMaskingProviderConfig configuration = new EmailMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new EmailMaskingProvider(configuration);

    String invalidEmail = "Invalid Email";
    String maskedEmail = maskingProvider.mask(invalidEmail);

    assertEquals(null, maskedEmail);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testPreserveDomain() {
    EmailMaskingProviderConfig configuration = new EmailMaskingProviderConfig();
    configuration.setPreserveDomains(0);

    String originalValue = "dummyEmail@ie.ibm.com";
    EmailMaskingProvider maskingProvider = new EmailMaskingProvider(configuration);

    int domainOK = 0;
    // Note: When preserveDomain is 0, the tld is masked randomly and
    // may/may not be the same as the original tld.
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(originalValue);
      assertThat(maskingProvider.mask("dummyEmail@ie.ibm.com"), not("dummyEmail@ie.ibm.com"));
      if (!maskedValue.endsWith(".com")) {
        domainOK++;
      }
      // System.out.println("=======> eMail preserveDomain = 0,
      // originValue ["
      // + originalValue + "], masked value [" + maskedValue + "]");
    }
    assertTrue(domainOK > 0);

    domainOK = 0;
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(originalValue);
      assertThat(maskingProvider.mask("dummyname@test.co.uk"), not("dummyname@test.co.uk"));
      if (!maskedValue.endsWith(".co.uk")) {
        domainOK++;
      }
      // System.out.println("=======> eMail preserveDomain = 0,
      // originValue ["
      // + originalValue + "], masked value [" + maskedValue + "]");
    }
    assertTrue(domainOK > 0);
  }

  @Test
  public void testPreserveSubDomain() {
    EmailMaskingProviderConfig configuration = new EmailMaskingProviderConfig();
    configuration.setPreserveDomains(2);

    String originalValue = "dummyEmail@ie.ibm.com";
    EmailMaskingProvider maskingProvider = new EmailMaskingProvider(configuration);

    String maskedValue = maskingProvider.mask(originalValue);

    // System.out.println("=======> eMail preserveDomain = 2: originValue ["
    // + originalValue + "], masked value [" + maskedValue + "]");

    assertTrue(maskedValue.endsWith(".ibm.com"));
  }

  @Test
  public void testRandomwithNumber() {
    EmailMaskingProviderConfig configuration = new EmailMaskingProviderConfig();
    configuration.setPreserveDomains(-1);
    configuration.setNameLength(8);

    String originalValue = "dummyEmail@ie.ibm.com";
    EmailMaskingProvider maskingProvider = new EmailMaskingProvider(configuration);
    String maskedValue = maskingProvider.mask(originalValue);
    String[] domain = maskedValue.split("[@._]");
    assertTrue(domain[0].length() == 8);
  }

  @Test
  public void testPreserveDomainLong() {
    EmailMaskingProviderConfig configuration = new EmailMaskingProviderConfig();
    configuration.setPreserveDomains(5);

    String originalValue = "dummyEmail@ie.ibm.com";
    EmailMaskingProvider maskingProvider = new EmailMaskingProvider(configuration);

    String maskedValue = maskingProvider.mask(originalValue);

    System.out.println("=======>  eMail preserveDomain = 2: originValue [" + originalValue
        + "], masked value [" + maskedValue + "]");
    assertTrue(maskedValue.endsWith("ie.ibm.com"));
  }

  @Test
  @Ignore
  public void testPerformance() {
    int N = 1000000;
    EmailMaskingProviderConfig defaultMaskingConfiguration = new EmailMaskingProviderConfig();
    defaultMaskingConfiguration.setPreserveDomains(0);
    EmailMaskingProviderConfig preserveConfiguration = new EmailMaskingProviderConfig();
    preserveConfiguration.setPreserveDomains(2);

    EmailMaskingProviderConfig[] configurations =
        new EmailMaskingProviderConfig[] {defaultMaskingConfiguration, preserveConfiguration};

    String originalValue = "dummyEmail@ie.ibm.com";

    for (EmailMaskingProviderConfig configuration : configurations) {
      EmailMaskingProvider maskingProvider = new EmailMaskingProvider(configuration);

      long startMillis = System.currentTimeMillis();

      for (int i = 0; i < N; i++) {
        maskingProvider.mask(originalValue);
      }

      long diff = System.currentTimeMillis() - startMillis;
      System.out.println(String.format(" %d operations took %d milliseconds (%f per op)", N, diff,
          (double) diff / N));
      // Assert test always should finish in less than 10 seconds
      assertTrue(diff < 10000);
    }
  }
}
