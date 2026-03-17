/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint.datamasking;

import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.ibm.whc.deid.shared.exception.DeidException;

public final class MaskingProviderOutputComparator {

  private static final Logger log = LoggerFactory.getLogger(MaskingProviderOutputComparator.class);

  public MaskingProviderOutputComparator() {

  }

  public Boolean CompareMap(Map<String, String> input, Map<String, String> output)
      throws DeidException, IOException {

    try {
      if (input == null || output == null || input.size() != output.size())
        return false;
      for (String key : input.keySet()) {
        String value1 = input.get(key);
        String value2 = output.get(key);
        switch (key) {
          case "swift":
            log.info("Swift test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Swift test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "phone":
            log.info("Phone test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Phone test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "continent":
            log.info("Continent test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Continent test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "city":
            log.info("City test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("City test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "country":
            log.info("Country test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Country test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "icd9":
            log.info("icd9 test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("ICD9 test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "icd10":
            // ICD10 returns null because we don't ship any ICDv10 codes by default due to
            // licensing rules, so there are no codes we can generate by default.
            log.info("icd10 test is " + testReturnsNull(value1, value2));
            assertTrue("ICD10 test in API test FAILED - check logs",
                testReturnsNull(value1, value2));
            break;
          case "marital":
            log.info("Marital test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Marital test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "address":
            log.info("Address test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Address test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "atc":
            log.info("ATC test is " + testATC(value1, value2));
            assertTrue("ATC test in API test FAILED - check logs", testATC(value1, value2));
            break;
          case "binning":
            log.info("Binning test is " + testBinning(value1, value2));
            assertTrue("Binning test in API test FAILED - check logs", testBinning(value1, value2));
            break;
          case "conditional":
            log.info("Conditional test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Conditional test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "county":
            log.info("County test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("County test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "credit_card":
            log.info("Credit card test is " + testReturnsRandomNumber(value1, value2));
            assertTrue("Credit card test in API test FAILED - check logs",
                testReturnsRandomNumber(value1, value2));
            break;
          case "dateDependency":
            log.info("Date dependency test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Date dependency test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "dateTime":
            log.info("Date time test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Date time test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "dateTimeConsistentShift":
            assertTrue("DateTime consistent shift in API test FAILED - check logs",
                testDateTimeConsistentShift(value1, value2));
            log.info(
                "DateTime consistent shift test is " + testDateTimeConsistentShift(value1, value2));
            break;
          case "email":
            log.info("Email test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Email test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "fpe":
            log.info("FPE test is " + testFPE(value1, value2));
            assertTrue("FPE test in API test FAILED - check logs", testFPE(value1, value2));
            break;
          case "gender":
            log.info("Gender test is " + testGender(value1, value2));
            assertTrue("Gender test in API test FAILED - check logs", testGender(value1, value2));
            break;
          case "generalize":
            log.info("Generalize test is " + testGeneralize(value1, value2));
            assertTrue("Generalize test in API test FAILED - check logs",
                testGeneralize(value1, value2));
            break;
          case "guid":
            log.info("Guid test is " + testKey(value1, value2));
            assertTrue("Guid test in API test FAILED - check logs", testKey(value1, value2));
            break;
          case "hash":
            log.info("Hash test is " + testHash(value1, value2));
            assertTrue("Hash test in API test FAILED - check logs", testHash(value1, value2));
            break;
          case "hospital":
            log.info("Hospital test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Hospital test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "iban":
            log.info("Iban test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Iban test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "imei":
            log.info("IMEI test is " + testReturnsRandomNumber(value1, value2));
            assertTrue("IMEI test in API test FAILED - check logs",
                testReturnsRandomNumber(value1, value2));
            break;
          case "ipAddress":
            log.info("IP Address test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("IP Address test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "latitudeLongitude":
            log.info("Latitude Longitude test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Latitude Longitude test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "macAddress":
            log.info("Mac address test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Mac address test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "maintain":
            log.info("Maintain test is " + testMaintain(value1, value2));
            assertTrue("Maintain test in API test FAILED - check logs",
                testMaintain(value1, value2));
            break;
          case "name":
            log.info("Name test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Name test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "null":
            log.info("Null test is " + testReturnsEmptyString(value1, value2));
            assertTrue("Null test in API test FAILED - check logs",
                testReturnsEmptyString(value1, value2));
            break;
          case "numberVariance":
            log.info("numberVariance test is " + testReturnsRandomNumber(value1, value2));
            assertTrue("Number variance test in API test FAILED - check logs",
                testReturnsRandomNumber(value1, value2));
            break;
          case "occupation":
            log.info("Occupation test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Occupation test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "pseudonym":
            log.info("pseudonym test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("pseudonym test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "race":
            log.info("Race test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Race test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "random":
            log.info("Random test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("Random test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "redact":
            log.info("Redact test is " + testRedact(value1, value2));
            assertTrue("Redact test in API test FAILED - check logs", testRedact(value1, value2));
            break;
          case "religion":
            log.info("Religion test is " + testReligion(value1, value2));
            assertTrue("Religion test in API test FAILED - check logs",
                testReligion(value1, value2));
            break;
          case "replace":
            log.info("Replace test is " + testReplace(value1, value2));
            assertTrue("Replace test in API test FAILED - check logs", testReplace(value1, value2));
            break;
          case "ssnUK":
            log.info("ssn UK test is " + testSSNUK(value1, value2));
            assertTrue("SSN UK test in API test FAILED - check logs", testSSNUK(value1, value2));
            break;
          case "ssnUS":
            log.info("ssn US test is " + testSSNUS(value1, value2));
            assertTrue("SSN US test in API test FAILED - check logs", testSSNUS(value1, value2));
            break;
          case "stateUS":
            log.info("State US test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("State US test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "url":
            log.info("URL test is " + testReturnsArbitaryValue(value1, value2));
            assertTrue("URL test in API test FAILED - check logs",
                testReturnsArbitaryValue(value1, value2));
            break;
          case "vin":
            log.info("Vin test is " + testVin(value1, value2));
            assertTrue("Vin test in API test FAILED - check logs", testVin(value1, value2));
            break;
          case "zipcode":
            log.info("Zipcode test is " + testZipcode(value1, value2));
            assertTrue("Zipcode test in API test FAILED - check logs", testZipcode(value1, value2));
            break;
          default:
            assertTrue("Could not find test for" + key, false);
        }
      }

      return true;
    } catch (Exception e) {
      log.error("Exception in Comparing data : " + e.getMessage(), e);
      throw e;
    }
  }

  // ATC test should have output as A02BC
  public boolean testATC(String i, String o) {

    if (o.equals("A02BC"))
      return true;
    return false;

  }

  // Binning test should have output in a range
  public boolean testBinning(String i, String o) {

    if (o.equals("4566455-4566460"))
      return true;
    return false;

  }

  // Gender should either be Female or Male
  public boolean testGender(String i, String o) {

    if (o.equals("Female") || o.equals("Male"))
      return true;
    return false;

  }

  // testDateTimeConsistentShift
  public boolean testDateTimeConsistentShift(String i, String o) {

    if (!i.equals(o)) {
      if (o.length() == 20)
        return true;
    } else if (o.equals("null"))
      return false;
    return false;

  }

  // For FPE random number generated in output should be same number of digits as
  // the input.
  public boolean testFPE(String i, String o) {

    if (i.length() == o.length()) {
      double d = Double.parseDouble(o);
      return true;
    }
    return false;

  }

  // Test the output returns the mentioned value defined in the rules
  public boolean testGeneralize(String i, String o) {

    if (o.equals("correct")) {

      return true;
    }
    return false;

  }

  // Hash : The output value should be of length 36
  public boolean testHash(String i, String o) {

    if (!i.equals(o)) {
      if (o.length() == 64) {
        return true;
      }
    }
    return false;

  }


  // Key : The output value should be of length 36 and have a arbitary key
  public boolean testKey(String i, String o) {

    if (!(i.equals(o)) && (o.length() == 36)) {
      String regex = "[a-z0-9]{8}+\\-[a-z0-9]{4}+\\-[a-z0-9]{4}+\\-[a-z0-9]{4}+\\-[a-z0-9]{12}";
      // Compiling the regular expression
      Pattern pattern = Pattern.compile(regex);
      // Retrieving the matcher object
      Matcher matcher = pattern.matcher(o);
      if (matcher.matches()) {
        return true;
      }
      return false;
    }
    return false;

  }

  // should get a string the same length as the input but all the characters are
  // X.
  public boolean testRedact(String i, String o) {

    if (i.length() == o.length()) {
      for (int j = 0; j < o.length(); j++) {
        if (o.charAt(j) == 'X') {
          continue;
        } else {
          return false;
        }
      }
      return true;
    }
    return false;

  }

  // Maintain, Input and output values should be same
  public boolean testMaintain(String i, String o) {

    if (i.equals(o))
      return true;
    return false;

  }

  // Replace, the length should be the same and prefix should be same for first 3 characters
  public boolean testReplace(String i, String o) {

    if ((i.length() == o.length()) && (!i.equals(o))) {
      if ((o.substring(0, 3)).equals(i.substring(0, 3)))
        return true;
    }
    return false;

  }

  // SSN UK, the length should be the same and prefix should start with ZA
  public boolean testSSNUK(String i, String o) {

    if ((i.length() == o.length()) && (!i.equals(o))) {
      if ((o.substring(0, 2)).equals(i.substring(0, 2)))
        return true;
    }
    return false;

  }

  // SSN US, the length should be the same and output should contain string in with 3 digits, a
  // dash, 2 digits, a dash, and four digits.
  public boolean testSSNUS(String i, String o) {

    if ((i.length() == o.length()) && (!i.equals(o))) {
      String regex = "\\d{3}+\\-\\d{2}+\\-\\d{4}";
      // Compiling the regular expression
      Pattern pattern = Pattern.compile(regex);
      // Retrieving the matcher object
      Matcher matcher = pattern.matcher(o);
      if (matcher.matches()) {
        return true;
      }
      return false;
    }
    return false;

  }

  // Vin, the length should be the same and prefix should be same for first 3 characters
  public boolean testVin(String i, String o) {

    if ((i.length() == o.length()) && (!i.equals(o))) {
      if ((o.substring(0, 3)).equals(i.substring(0, 3)))
        return true;
    }
    return false;

  }

  // zipcode, the length should be the same and prefix should be same for first 3 characters
  public boolean testZipcode(String i, String o) {

    if (o.equals(i.substring(0, 3)))
      return true;

    return false;

  }

  // This test checks if the output has null value
  public boolean testReturnsNull(String i, String o) {

    if (o == null)
      return true;
    else
      return false;

  }

  // This test checks if the output has no value or an empty string
  public boolean testReturnsEmptyString(String i, String o) {

    if (o.equals(""))
      return true;
    else
      return false;

  }

  // This test checks that input value is different from the output value
  public boolean testReturnsArbitaryValue(String i, String o) {

    if (i.equals(o))
      return false;
    else if (o == null)
      return false;
    else if (o.equals("null"))
      return false;
    else
      return true;

  }

  // This test checks that the output should contain only numbers which should be
  // different from the input value
  public boolean testReturnsRandomNumber(String i, String o) {

    if (i.equals(o)) {
      return false;
    } else if (o != null) {
      try {
        double d = Double.parseDouble(o);
        return true;
      } catch (NumberFormatException nfe) {
        return false;
      }
    } else
      return false;
  }

  // Religion output should only contain the few mentioned values
  public boolean testReligion(String i, String o) {

    if (o.equals("Hinduism") || o.equals("Hindu") || o.equals("Buddhism") || o.equals("Buddhist")
        || o.equals("Christianity") || o.equals("Christian") || o.equals("Islam")
        || o.equals("Muslim") || o.equals("Shiite") || o.equals("Sunni") || o.equals("Catholicism")
        || o.equals("Catholic") || o.equals("Protestant") || o.equals("Orthodox")
        || o.equals("Judaism") || o.equals("Sikhism") || o.equals("Taoism") || o.equals("Taoist")
        || o.equals("Shinto") || o.equals("Confucianism") || o.equals("Zoroastrianism"))
      return true;
    return false;

  }
}
