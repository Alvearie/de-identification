/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import com.ibm.whc.deid.models.PhoneNumber;
import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.util.MSISDNManager;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * The type Phone identifier.
 *
 */
public class PhoneIdentifier extends AbstractIdentifier {

  /** */
  private static final long serialVersionUID = -5245668163814340807L;

  private static final String[] appropriateNames =
      {"Phone Number", "Mobile", "Mobile Number", "Telephone", "Tel."};

  /** The constant msisdnManager. */
  public static final MSISDNManager msisdnManager = new MSISDNManager(null);

  private final static LogManager log = LogManager.getInstance();

  // Provide some default patterns to recognize phone numbers
  public static final List<String> DEFAULT_PATTERN_STRINGS;

  private static final Pattern[] defaultPatterns;
  static {
    String[] defaultPatternStrings =
        {
        "^(?<prefix>\\+|00)(?<countryCode>\\d{1,3})(?<separator>-| )(?<number>\\d+)",                 // +353-0876653255
        "^(?<prefix>\\+|00)(?<countryCode>\\d{1,3})(?<separator>-| )(?<number>\\(\\d+\\))\\d+"};      // +353-(087)6653255   
    DEFAULT_PATTERN_STRINGS = Collections.unmodifiableList(Arrays.asList(defaultPatternStrings));
    defaultPatterns = new Pattern[defaultPatternStrings.length];
    for (int i = 0; i < defaultPatternStrings.length; i++) {
      defaultPatterns[i] = Pattern.compile(defaultPatternStrings[i]);
    }
  }

  // allow users to provide custom patterns.
  private final Pattern[] customPatterns;

  public PhoneIdentifier(List<String> customRegexPatterns) {
    if (customRegexPatterns == null) {
      customPatterns = null;
    } else {
      customPatterns = new Pattern[customRegexPatterns.size()];
      customRegexPatterns.stream().map(x -> Pattern.compile(x)).collect(Collectors.toList())
          .toArray(customPatterns);
    }
  }

  @Override
  public ProviderType getType() {
    return ProviderType.PHONE;
  }

  /**
   * Gets phone number.
   *
   * @param identifier the identifier
   * @return the phone number
   */
  public PhoneNumber getPhoneNumber(String identifier) {
    Pattern matchingPattern = getMatchingPattern(identifier);
    if (matchingPattern == null) {
      return null;
    }

    Matcher m = matchingPattern.matcher(identifier);
    if (!m.matches()) {
      return null;
    }

    String prefix = getMatchedGroup(m, "prefix");

    String countryCode = getMatchedGroup(m, "countryCode");
    if (countryCode != null && !msisdnManager.isValidCountryCode(countryCode)) {
      return null;
    }

    String separator = getMatchedGroup(m, "separator");
    String number = getMatchedGroup(m, "number");

    String areaCode = getMatchedGroup(m, "areaCode");
    if (number == null) {
      if (log.isWarnEnabled()) {
        log.logWarn(LogCodes.WPH1012W, "number is null");
      }
      return null;
    }
    if (areaCode == null && countryCode != null && countryCode.equals("1")) {
      areaCode = number.substring(0, 3);
      number = number.substring(3);
    }

    return new PhoneNumber(prefix, countryCode, separator, number, areaCode, prefix != null);
  }

  /**
   * Get the patterns used to match phone patterns
   *
   * @return
   */
  private Pattern[] getPatterns() {
    if (customPatterns == null) {
      return defaultPatterns;
    }
    return customPatterns;
  }

  /**
   * Get matched named group.
   *
   * @param matcher
   * @return
   */
  private String getMatchedGroup(Matcher matcher, String name) {
    try {
      return matcher.group(name);
    } catch (IllegalArgumentException e) {
      // The pattern did not include a country code. Just return true
      return null;
    }
  }

  /**
   * Gets matching pattern.
   *
   * @param data the data
   * @return the matching pattern
   */
  public Pattern getMatchingPattern(String data) {
    Pattern[] patterns = getPatterns();

    for (Pattern pattern : patterns) {
      Matcher m = pattern.matcher(data);
      if (!m.matches()) {
        continue;
      }
      return pattern;
    }

    return null;
  }

  @Override
  public boolean isOfThisType(String data) {
    if (getMatchingPattern(data) != null) {
      return true;
    }

    if (msisdnManager.isValidUSNumber(data)) {
      return true;
    }

    return false;
  }

  @Override
  public String getDescription() {
    return "Phone number identification. Supports international formats with country code detection";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }
}
