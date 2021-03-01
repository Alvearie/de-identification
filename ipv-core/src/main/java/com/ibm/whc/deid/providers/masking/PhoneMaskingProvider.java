/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import java.util.List;

import com.ibm.whc.deid.models.PhoneNumber;
import com.ibm.whc.deid.providers.identifiers.PhoneIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.PhoneMaskingProviderConfig;
import com.ibm.whc.deid.util.MSISDNManager;
import com.ibm.whc.deid.util.RandomGenerators;

/**
 * The type Phone masking provider.
 *
 */
public class PhoneMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -8562773893937629362L;

  protected MSISDNManager msisdnManager;
  private final boolean preserveCountryCode;
  private final boolean preserveAreaCode;
  private final String invNdigitsReplaceWith;
  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;
  private final List<String> phoneRegexPatterns;

  private final PhoneIdentifier phoneIdentifier;

  protected volatile boolean initialized = false;

  public PhoneMaskingProvider(PhoneMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty);
    this.random = new SecureRandom();
    this.preserveCountryCode = configuration.isCountryCodePreserve();
    this.preserveAreaCode = configuration.isAreaCodePreserve();
    this.invNdigitsReplaceWith = configuration.getInvNdigitsReplaceWith();
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
    this.phoneRegexPatterns = configuration.getPhoneRegexPatterns();
    this.phoneIdentifier =
        new PhoneIdentifier(this.phoneRegexPatterns, tenantId, localizationProperty);
  }

  private String generateRandomPhoneNumber(String countryCode) {
    String separator = "-";
    int countryCodeDigits;
    if (msisdnManager.isCountryWithValidDigitMap(countryCode)) {
      countryCodeDigits = msisdnManager.getRandomPhoneNumberDigitsByCountry(countryCode);
    } else {
      countryCode = "1";
      countryCodeDigits = msisdnManager.getRandomPhoneNumberDigitsByCountry("1");
    }
    String phoneNumber = RandomGenerators.generateRandomDigitSequence(countryCodeDigits);

    return "+" + countryCode + separator + phoneNumber;
  }

  /**
   * Mask the country code portion of a phone number.
   *
   * @param phoneNumber
   * @return
   */
  private String maskCountryCode(PhoneNumber phoneNumber) {
    String countryCode;
    if (this.preserveCountryCode || phoneNumber.getCountryCode() == null) {
      countryCode = phoneNumber.getCountryCode();
    } else {
      countryCode = msisdnManager.getRandomCountryCode();
    }

    return countryCode;
  }

  /**
   * Mask the area code portion of a phone number.
   *
   * @param phoneNumber
   * @return
   */
  private String maskAreaCode(PhoneNumber phoneNumber) {
    String originalAreaCode = phoneNumber.getAreaCode();
    if (originalAreaCode == null) {
      return null;
    }

    String areaCode;
    if (this.preserveAreaCode) {
      areaCode = phoneNumber.getAreaCode();
    } else {
      // Randomize area code
      int areaCodeLength = originalAreaCode.length();
      StringBuilder builder = new StringBuilder(areaCodeLength);

      for (int i = 0; i < areaCodeLength; ++i) {
        char c = originalAreaCode.charAt(i);

        if (Character.isDigit(c)) {
          c = (char) ('0' + random.nextInt(9));
        }
        builder.append(c);
      }

      areaCode = builder.toString();
    }

    return areaCode;
  }

  private String maskPhoneNumberDigits(PhoneNumber phoneNumber, String countryCode) {
    String originalDigits = phoneNumber.getNumber();
    if (originalDigits == null) {
      return null;
    }

    StringBuilder sb = new StringBuilder();

    // If the original phone number does not have a country code or the
    // country code is the same,
    // use the original phone number to mask so that we can keep the
    // format(spaces, characters)
    if (countryCode == null || preserveCountryCode) {

      int length = originalDigits.length();

      for (int i = 0; i < length; i++) {
        char c = originalDigits.charAt(i);
        // if the character is a digit, randomized it.
        if (Character.isDigit(c)) {
          sb.append((char) ('0' + random.nextInt(9)));
        } else {
          // for non-digits, assume they are symbols or spaces, just
          // keep
          // it.
          sb.append(c);
        }
      }
    } else {
      int phoneNumberDigits;
      if (!msisdnManager.isCountryWithValidDigitMap(countryCode)) {
        countryCode = "1";
      }
      phoneNumberDigits = msisdnManager.getRandomPhoneNumberDigitsByCountry(countryCode);
      for (int idx = 0; idx < phoneNumberDigits; idx++) {
        char c = (char) ('0' + random.nextInt(10));
        sb.append(c);
      }
    }
    return sb.toString();
  }

  @Override
  public String mask(String identifier) {
    initialize();
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    PhoneNumber phoneNumber = phoneIdentifier.getPhoneNumber(identifier);
    if (phoneNumber == null) {
      debugFaultyInput("phoneNumber");
      if (unspecifiedValueHandling == 2) {
        return generateRandomPhoneNumber(invNdigitsReplaceWith);
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    String maskedCountryCode = maskCountryCode(phoneNumber);

    String maskedAreaCode = maskAreaCode(phoneNumber);

    String maskedPhoneNumberDigits = maskPhoneNumberDigits(phoneNumber, maskedCountryCode);

    String maskedPhone = identifier;

    if (maskedCountryCode != null) {
      maskedPhone = maskedPhone.replace(phoneNumber.getCountryCode(), maskedCountryCode);
    }

    if (maskedAreaCode != null) {
      maskedPhone = maskedPhone.replace(phoneNumber.getAreaCode(), maskedAreaCode);
    }

    if (maskedPhoneNumberDigits != null) {
      maskedPhone = maskedPhone.replace(phoneNumber.getNumber(), maskedPhoneNumberDigits);
    }

    return maskedPhone;
  }

  protected void initialize() {
    if (!initialized) {
      msisdnManager = new MSISDNManager(null, localizationProperty);
      initialized = true;
    }
  }
}
