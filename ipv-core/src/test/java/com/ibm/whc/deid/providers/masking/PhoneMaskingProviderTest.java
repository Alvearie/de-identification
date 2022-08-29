/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.List;
import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.ibm.whc.deid.models.PhoneNumber;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.PhoneIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.PhoneMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class PhoneMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {
  private static final Logger log = LoggerFactory.getLogger(PhoneMaskingProviderTest.class);

  private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

  @Test
  public void testMask() {
    PhoneMaskingProviderConfig configuration = new PhoneMaskingProviderConfig();
    PhoneMaskingProvider phoneMaskingProvider =
        new PhoneMaskingProvider(configuration, tenantId, localizationProperty);
    PhoneIdentifier phoneIdentifier = new PhoneIdentifier(null, tenantId, localizationProperty);

    String originalPhone = "+353-0876653255";
    PhoneNumber originalPhoneNumber = phoneIdentifier.getPhoneNumber(originalPhone);

    for (int i = 0; i < 1000; i++) {
      String maskedPhone = phoneMaskingProvider.mask(originalPhone);

      PhoneNumber maskedPhoneNumber = phoneIdentifier.getPhoneNumber(maskedPhone);

      assertTrue(originalPhoneNumber.getPrefix().equals(maskedPhoneNumber.getPrefix()));
      assertTrue(originalPhoneNumber.getSeparator().equals(maskedPhoneNumber.getSeparator()));

      assertTrue(originalPhoneNumber.getCountryCode().equals(maskedPhoneNumber.getCountryCode()));
      assertFalse(originalPhoneNumber.getNumber().equals(maskedPhoneNumber.getNumber()));
    }

    // case we dont preserve the country code
    configuration = new PhoneMaskingProviderConfig();
    configuration.setCountryCodePreserve(false);

    phoneMaskingProvider = new PhoneMaskingProvider(configuration, tenantId, localizationProperty);

    int randomizationOKCC = 0;
    for (int i = 0; i < 1000; i++) {
      String maskedPhone = phoneMaskingProvider.mask(originalPhone);
      PhoneNumber maskedPhoneNumber = phoneIdentifier.getPhoneNumber(maskedPhone);
      if (maskedPhoneNumber == null || maskedPhoneNumber.getCountryCode() == null
          || originalPhoneNumber.getCountryCode() == null) {
        phoneIdentifier.getPhoneNumber(maskedPhone);
      }
      if (!originalPhoneNumber.getCountryCode().equals(maskedPhoneNumber.getCountryCode())) {
        randomizationOKCC++;
      }
    }

    assertTrue(randomizationOKCC > 0);
  }

  @Test
  public void testMaskUSNumberNoAreaCode() throws Exception {
    PhoneMaskingProviderConfig configuration = new PhoneMaskingProviderConfig();
    configuration.setAreaCodePreserve(false);
    PhoneMaskingProvider phoneMaskingProvider =
        new PhoneMaskingProvider(configuration, tenantId, localizationProperty);

    String originalPhone = "+1-3471234567";

    int randomOK = 0;

    for (int i = 0; i < 1000; i++) {
      String maskedPhone = phoneMaskingProvider.mask(originalPhone);
      assertFalse(originalPhone.equals(maskedPhone));

      if (!maskedPhone.startsWith("+1-347")) {
        randomOK++;
      }
    }

    assertTrue(randomOK > 0);
  }

  @Ignore
  @Test
  public void testPerformance() {
    int N = 1000000;
    PhoneMaskingProviderConfig defaultConfiguration = new PhoneMaskingProviderConfig();

    PhoneMaskingProviderConfig[] configurations =
        new PhoneMaskingProviderConfig[] {defaultConfiguration};

    String[] originalValues = new String[] {"+353-0876653255", "3471234567"};

    for (PhoneMaskingProviderConfig maskingConfiguration : configurations) {
      PhoneMaskingProvider maskingProvider =
          new PhoneMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

      for (String originalValue : originalValues) {
        long startMillis = System.currentTimeMillis();

        for (int i = 0; i < N; i++) {
          maskingProvider.mask(originalValue);
        }

        long diff = System.currentTimeMillis() - startMillis;
        System.out.println(String.format(" %s: %d operations took %d milliseconds (%f per op)",
            originalValue, N, diff, (double) diff / N));
        // Assert test always should finish in less than 2 minutes
        assertTrue(diff < 120000);
      }
    }
  }

  @Test
  public void testMaskNullPhoneInputReturnNull() throws Exception {
    PhoneMaskingProviderConfig configuration = new PhoneMaskingProviderConfig();
    MaskingProvider maskingProvider =
        new PhoneMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidPhone = null;
    String maskedPhone = maskingProvider.mask(invalidPhone);

    assertEquals(null, maskedPhone);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidPhoneInputValidHandlingReturnNull() throws Exception {
    PhoneMaskingProviderConfig configuration = new PhoneMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    MaskingProvider maskingProvider =
        new PhoneMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidPhone = "Invalid Phone";
    String maskedPhone = maskingProvider.mask(invalidPhone);

    assertEquals(null, maskedPhone);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidPhoneInputValidHandlingReturnRandom() throws Exception {
    PhoneMaskingProviderConfig configuration = new PhoneMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    MaskingProvider maskingProvider =
        new PhoneMaskingProvider(configuration, tenantId, localizationProperty);
    Identifier identifier = new PhoneIdentifier(null, tenantId, localizationProperty);

    String invalidPhone = "Invalid Phone";
    String maskedPhone = maskingProvider.mask(invalidPhone);

    assertFalse(maskedPhone.equals(invalidPhone));
    assertTrue(identifier.isOfThisType(maskedPhone));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidPhoneInputValidHandling_ValidNumber_InvalidNumDigits()
      throws Exception {
    PhoneMaskingProviderConfig configuration = new PhoneMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    // Cuba country code
    configuration.setInvNdigitsReplaceWith("53");
    PhoneMaskingProvider maskingProvider =
        new PhoneMaskingProvider(configuration, tenantId, localizationProperty);
    Identifier identifier = new PhoneIdentifier(null, tenantId, localizationProperty);

    String invalidPhone = "+53-08766532550864345656";
    String maskedPhone = maskingProvider.mask(invalidPhone);

    assertFalse(maskedPhone.equals(invalidPhone));
    assertTrue(identifier.isOfThisType(maskedPhone));
    assertFalse(maskingProvider.getMSISDNManager().isValidCountryNumDigits("53",
        maskedPhone.split("-")[1].length()));
  }

  @Test
  public void testMaskInvalidPhoneInputValidHandling_InvalidNumDigitsValidReplacement()
      throws Exception {
    PhoneMaskingProviderConfig configuration = new PhoneMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    // Spain country code
    configuration.setInvNdigitsReplaceWith("34");
    PhoneMaskingProvider maskingProvider =
        new PhoneMaskingProvider(configuration, tenantId, localizationProperty);
    Identifier identifier = new PhoneIdentifier(null, tenantId, localizationProperty);

    String invalidPhone = "Invalid Phone";
    String maskedPhone = maskingProvider.mask(invalidPhone);

    assertFalse(maskedPhone.equals(invalidPhone));
    assertTrue(identifier.isOfThisType(maskedPhone));
    assertTrue(maskingProvider.getMSISDNManager().isValidCountryNumDigits("34",
        maskedPhone.split("-")[1].length()));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidPhoneInputValidHandling_InvalidNumDigitsInvalidReplacement()
      throws Exception {
    PhoneMaskingProviderConfig configuration = new PhoneMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    // Invalid country code
    configuration.setInvNdigitsReplaceWith("538");
    PhoneMaskingProvider maskingProvider =
        new PhoneMaskingProvider(configuration, tenantId, localizationProperty);
    Identifier identifier = new PhoneIdentifier(null, tenantId, localizationProperty);

    String invalidPhone = "Invalid Phone";
    String maskedPhone = maskingProvider.mask(invalidPhone);
    assertFalse(maskedPhone.equals(invalidPhone));

    assertTrue(identifier.isOfThisType(maskedPhone));

    assertTrue(maskingProvider.getMSISDNManager().isValidCountryNumDigits("1",
        maskedPhone.split("-")[1].length()));

    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidPhoneInputValidHandlingReturnDefaultCustomValue() throws Exception {
    PhoneMaskingProviderConfig configuration = new PhoneMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    MaskingProvider maskingProvider =
        new PhoneMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidPhone = "Invalid Phone";
    String maskedPhone = maskingProvider.mask(invalidPhone);

    assertEquals("OTHER", maskedPhone);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidPhoneInputValidHandlingReturnNonDefaultCustomValue() throws Exception {
    PhoneMaskingProviderConfig configuration = new PhoneMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    configuration.setUnexpectedInputReturnMessage("Test Phone");
    MaskingProvider maskingProvider =
        new PhoneMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidPhone = "Invalid Phone";
    String maskedPhone = maskingProvider.mask(invalidPhone);

    assertEquals("Test Phone", maskedPhone);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testFrancePhoneNumber() {
    PhoneMaskingProviderConfig configuration = new PhoneMaskingProviderConfig();
    List<String> phoneRegexPatterns = new ArrayList<>();
    // mobile number 10 digits
    phoneRegexPatterns.add("^(?<number>\\d{2} \\d{2} \\d{2} \\d{2} \\d{2})");

    configuration.setPhoneRegexPatterns(phoneRegexPatterns);

    MaskingProvider maskingProvider =
        new PhoneMaskingProvider(configuration, tenantId, localizationProperty);

    String phoneNumber = "02 43 87 23 05";
    String maskedPhoneNumber = maskingProvider.mask(phoneNumber);
    log.info("Phone  original: {}     masked: {}", phoneNumber, maskedPhoneNumber);
    assertNotEquals(phoneNumber, maskedPhoneNumber);
  }

  /** Test various format */
  @Test
  public void testFormat() {

    PhoneMaskingProviderConfig configuration = new PhoneMaskingProviderConfig();
    List<String> phoneRegexPatterns = new ArrayList<>();
    // "+380 44 123 45 67"
    phoneRegexPatterns
        .add("^\\+(?<countryCode>\\d{1,3}) (?<areaCode>\\d{2,4}) (?<number>[0-9\\s]{8,9})");

    // another format "(044) 123 45 67"
    phoneRegexPatterns.add("^(?<areaCode>\\(\\d{2,4}\\)) (?<number>[0-9\\s]{8,9})");

    // another format "+380 (44) 1234567"
    phoneRegexPatterns
        .add("^\\+(?<countryCode>\\d{1,3}) (?<areaCode>\\(\\d{2,4}\\)) (?<number>[0-9]{7})");

    configuration.setPhoneRegexPatterns(phoneRegexPatterns);

    configuration.setAreaCodePreserve(false);

    MaskingProvider maskingProvider =
        new PhoneMaskingProvider(configuration, tenantId, localizationProperty);

    String internationalFormat = "+380 44 123 45 67";
    String maskedInternalFormat = maskingProvider.mask(internationalFormat);
    log.info("international  original: {}     masked: {}", internationalFormat,
        maskedInternalFormat);
    assertNotEquals(internationalFormat, maskedInternalFormat);
    assertNotNull(maskedInternalFormat);

    String format2 = "(044) 123 45 67";
    String maskedFormat2 = maskingProvider.mask(format2);
    log.info("format2  original: {}     masked: {}", format2, maskedFormat2);
    assertNotEquals(format2, maskedFormat2);
    assertEquals(15, maskedFormat2.length());
    assertNotNull(maskedFormat2);

    String anotherFormat = "+380 (44) 1234567";
    String maskedAnotherFormat = maskingProvider.mask(anotherFormat);
    log.info("another format  original: {}     masked: {}", anotherFormat, maskedAnotherFormat);
    assertNotEquals(anotherFormat, maskedAnotherFormat);
    assertNotNull(maskedAnotherFormat);
  }

  @Test
  public void testIndiaPhoneNumber() {
    PhoneMaskingProviderConfig configuration = new PhoneMaskingProviderConfig();
    List<String> phoneRegexPatterns = new ArrayList<>();
    // mobile number 10 digits
    phoneRegexPatterns.add("^(?<number>\\d{10})");

    phoneRegexPatterns.add("^(?<areaCode>\\(\\d{3}\\)) (?<number>\\d{4}-\\d{4})");

    configuration.setPhoneRegexPatterns(phoneRegexPatterns);

    MaskingProvider maskingProvider =
        new PhoneMaskingProvider(configuration, tenantId, localizationProperty);

    String mobileNumber = "7881342113";
    String maskedMobileNumber = maskingProvider.mask(mobileNumber);
    log.info("Mobile original: {}     masked: {}", mobileNumber, maskedMobileNumber);
    assertNotEquals(mobileNumber, maskedMobileNumber);

    String landLine = "(011) 2987-3456";
    String maskedLandLine = maskingProvider.mask(landLine);
    log.info("Landline original: {}     masked: {}", landLine, maskedLandLine);
    assertNotEquals(landLine, maskedLandLine);
    assertTrue(maskedLandLine.startsWith("(011) "));

    configuration.setAreaCodePreserve(false);

    maskingProvider = new PhoneMaskingProvider(configuration, tenantId, localizationProperty);
    boolean foundChanged = false;
    // try a few times since could happen randomly
    for (int i = 0; i < 10; i++) {
      maskedLandLine = maskingProvider.mask(landLine);
      log.info("Landline original: {}     masked: {}", landLine, maskedLandLine);
      assertNotEquals(landLine, maskedLandLine);
      // Make sure area code is changed
      foundChanged = !maskedLandLine.startsWith("(011) ");
      if (foundChanged) {
        break;
      }
    }
    assertTrue(foundChanged);

  }

  @Test
  public void testChinesePhoneNumber() {
    PhoneMaskingProviderConfig configuration = new PhoneMaskingProviderConfig();

    List<String> phoneRegexPatterns = new ArrayList<>();

    // mobile number 139 1099 8888
    phoneRegexPatterns.add("^(?<number>\\d{3} \\d{4} \\d{4})");

    // with country and area code +86 10 6552 9988
    phoneRegexPatterns.add(
        "^(?<prefix>\\+)(?<countryCode>\\d{1,2}) (?<areaCode>\\d{2,4}) (?<number>\\d{4} \\d{4})");

    // shared pay 4008101818-5200
    phoneRegexPatterns.add("^400(?<number>\\d{7}-\\d{4})");
    configuration.setPhoneRegexPatterns(phoneRegexPatterns);

    MaskingProvider maskingProvider =
        new PhoneMaskingProvider(configuration, tenantId, localizationProperty);

    String mobileNumber = "139 1099 8888";
    String maskedMobileNumber = maskingProvider.mask(mobileNumber);
    log.info("Mobile original: {}     masked: {}", mobileNumber, maskedMobileNumber);
    assertNotEquals(mobileNumber, maskedMobileNumber);

    String withCountryAreaCode = "+86 10 6552 9988";
    String maskedNumber1 = maskingProvider.mask(withCountryAreaCode);
    log.info("with country/area code original: {}     masked: {}", withCountryAreaCode,
        maskedNumber1);
    assertTrue(maskedNumber1.startsWith("+86 10 "));
    assertNotEquals(withCountryAreaCode, maskedNumber1);

    String sharedPay = "4008101818-5200";
    String maskedSharedPay = maskingProvider.mask(sharedPay);
    log.info("shared pay original: {}     masked: {}", sharedPay, maskedSharedPay);
    assertTrue(maskedSharedPay.startsWith("400"));
    assertNotEquals(sharedPay, maskedSharedPay);
  }

}
