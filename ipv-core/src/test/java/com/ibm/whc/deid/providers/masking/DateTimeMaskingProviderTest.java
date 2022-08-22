/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.time.temporal.TemporalField;
import java.util.Date;
import java.util.TreeMap;
import java.util.regex.Pattern;
import org.apache.commons.lang.StringUtils;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.DateTimeIdentifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;

public class DateTimeMaskingProviderTest extends TestLogSetUp {
  private static final String DATE_TIME_FORMAT = "dd-MM-yyyy HH:mm:ss";
  private static final int NUM_LOOP_NON_PERF_TEST = 10;

  /*
   * Tests all Datetime options. It tests various values for numerical and format options; however,
   * it ignores the generalize weekYear option check because of Java inconsistencies in
   * WEEK_OF_YEAR. The year, month, day...seconds mask boolean options, when set to true, are used
   * to adjust the mask by the specified upward and downward values, these flags are not applicable
   * when they are false and therefore no test case is defined for false value. It also tests for an
   * compound masking and invalid value.
   */
  @Test
  public void testMask() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    // different values
    String originalDateTime = "08-12-1981 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);
    assertFalse(originalDateTime.equals(maskedDateTime));

    originalDateTime = "08-12-1981";
    maskedDateTime = maskingProvider.mask(originalDateTime);
    assertFalse(originalDateTime.equals(maskedDateTime));
  }

  @Test
  public void testMaskShiftDate() throws Exception {
    DateTimeMaskingProviderConfig config = new DateTimeMaskingProviderConfig();
    config.setMaskShiftDate(true);
    config.setMaskShiftSeconds(120);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(config);

    // different values
    String originalDateTime = "08-12-1981 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);
    assertFalse(originalDateTime.equals(maskedDateTime));

    assertEquals("08-12-1981 00:02:00", maskedDateTime);
  }

  @Test
  @Ignore
  public void testMaskShiftDatePerformance() throws Exception {
    int N = 1000000;

    DateTimeMaskingProviderConfig config = new DateTimeMaskingProviderConfig();
    config.setMaskShiftDate(true);
    config.setMaskShiftSeconds(120);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(config);

    // different values
    String originalDateTime = "08-12-1981 00:00:00";

    long startMillis = System.currentTimeMillis();

    for (int i = 0; i < N; i++) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertEquals("08-12-1981 00:02:00", maskedDateTime);
    }

    long diff = System.currentTimeMillis() - startMillis;
    System.out.println(String.format(" %d operations took %d milliseconds (%f per op)", N, diff,
        (double) diff / N));
  }

  @Test
  public void testMaskShiftDateNegative() throws Exception {

    DateTimeMaskingProviderConfig config = new DateTimeMaskingProviderConfig();
    config.setFormatFixed("  \t\n  "); // whitespace, so ignored
    config.setMaskShiftDate(true);
    config.setMaskShiftSeconds(-120);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(config);

    // different values
    String originalDateTime = "08-12-1981 00:04:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);
    assertEquals("08-12-1981 00:02:00", maskedDateTime);
  }

  @Test
  public void testMaskFixedFormat() throws Exception {

    DateTimeMaskingProviderConfig config = new DateTimeMaskingProviderConfig();
    config.setFormatFixed("MM-dd-yyyy");
    config.setYearMask(false);
    config.setDayMask(false);
    config.setHourMask(false);
    config.setMinutesMask(false);
    config.setSecondsMask(false);
    config.setMonthMask(true);
    config.setMonthRangeDown(0);
    config.setMonthRangeUp(2);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(config);

    String originalDateTime = "08-22-1981";// 00:00:00";
    boolean changed = false;
    for (int i = 0; i < 10; i++) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertNotNull(maskedDateTime);
      assertTrue(maskedDateTime.equals("08-22-1981") || maskedDateTime.equals("09-22-1981")
          || maskedDateTime.equals("10-22-1981"));
      if (!maskedDateTime.equals(originalDateTime)) {
        changed = true;
      }
    }
    assertTrue(changed);
  }

  @Test
  public void testMaskFixedFormat_year() throws Exception {

    DateTimeMaskingProviderConfig config = new DateTimeMaskingProviderConfig();
    config.setFormatFixed("yyyy");
    config.setYearMask(true);
    config.setDayMask(false);
    config.setHourMask(false);
    config.setMinutesMask(false);
    config.setSecondsMask(false);
    config.setMonthMask(false);
    config.setYearRangeDown(2);
    config.setYearRangeUp(2);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(config);

    String originalDateTime = "1981";
    boolean changed = false;
    for (int i = 0; i < 20; i++) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertNotNull(maskedDateTime);
      int year = Integer.parseInt(maskedDateTime);
      assertTrue(1979 <= year && year <= 1983);
      if (!maskedDateTime.equals(originalDateTime)) {
        changed = true;
      }
    }
    assertTrue(changed);
  }

  @Test
  public void testMaskFixedFormat_badInput() throws Exception {

    DateTimeMaskingProviderConfig config = new DateTimeMaskingProviderConfig();
    config.setFormatFixed("MM-dd-yyyy");
    config.setYearMask(false);
    config.setDayMask(false);
    config.setHourMask(false);
    config.setMinutesMask(false);
    config.setSecondsMask(false);
    config.setMonthMask(true);
    config.setMonthRangeDown(0);
    config.setMonthRangeUp(2);
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(config);

    String originalDateTime = "08-22-1981 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);
    assertEquals("OTHER", maskedDateTime);
  }

  @Test
  public void testMaskFixedFormat_badInput_random() throws Exception {

    // The input does not match the fixed datetime pattern.
    // Apply unexpected input handling.
    // Configured option is random.
    // Since pattern was valid, generate a random value using that pattern.
    // Since pattern is MM-dd-yyyy, the random value must start with a month number

    DateTimeMaskingProviderConfig config = new DateTimeMaskingProviderConfig();
    config.setFormatFixed("MM-dd-yyyy");
    config.setYearMask(false);
    config.setDayMask(false);
    config.setHourMask(false);
    config.setMinutesMask(false);
    config.setSecondsMask(false);
    config.setMonthMask(true);
    config.setMonthRangeDown(0);
    config.setMonthRangeUp(2);
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(config);

    String originalDateTime = "18-22-1981";
    for (int i = 0; i < 100; i++) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertNotNull(maskedDateTime);
      int month = Integer.parseInt(maskedDateTime.substring(0, 2));
      assertTrue(Integer.toString(month), month >= 1 && month <= 12);
      assertEquals("-", maskedDateTime.substring(2, 3));
    }
  }

  @Test
  public void testMaskFixedFormat_badPattern() throws Exception {

    DateTimeMaskingProviderConfig config = new DateTimeMaskingProviderConfig();
    config.setFormatFixed("TTT");
    config.setYearMask(false);
    config.setDayMask(false);
    config.setHourMask(false);
    config.setMinutesMask(false);
    config.setSecondsMask(false);
    config.setMonthMask(true);
    config.setMonthRangeDown(0);
    config.setMonthRangeUp(2);
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    config.setUnexpectedInputReturnMessage("badPattern");

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(config);

    String originalDateTime = "18-22-1981";
    String maskedDateTime = maskingProvider.mask(originalDateTime);
    assertEquals("badPattern", maskedDateTime);
  }

  @Test
  public void testMaskFixedFormat_badPattern_random() throws Exception {

    DateTimeMaskingProviderConfig config = new DateTimeMaskingProviderConfig();
    config.setFormatFixed("TTT");
    config.setYearMask(false);
    config.setDayMask(false);
    config.setHourMask(false);
    config.setMinutesMask(false);
    config.setSecondsMask(false);
    config.setMonthMask(true);
    config.setMonthRangeDown(0);
    config.setMonthRangeUp(2);
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(config);

    String originalDateTime = "18-22-1981";
    Pattern pattern = Pattern.compile("\\d{2}/\\d{2}/\\d{4}$");
    for (int i = 0; i < 100; i++) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertTrue(maskedDateTime, pattern.matcher(maskedDateTime).matches());
    }
  }

  @Test
  public void testMaskAlphabetFixedFormat() {

    DateTimeMaskingProviderConfig config = new DateTimeMaskingProviderConfig();
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(config);

    String originalDateTime = "08-JAn-1981";
    String maskedDateTime = maskingProvider.mask(originalDateTime);
    System.out.println(maskedDateTime);
    assertFalse(originalDateTime.equals(maskedDateTime));
  }

  @Test
  public void testMaskFixedFormatNoChange() {
    DateTimeMaskingProviderConfig config = new DateTimeMaskingProviderConfig();
    config.setFormatFixed("dd-MM-yyyy HH:mm:ss");
    config.setYearMask(false);
    config.setMonthMask(false);
    config.setDayMask(false);
    config.setHourMask(false);
    config.setMinutesMask(false);
    config.setSecondsMask(false);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(config);
    String originalDateTime = "08-12-1981 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);
    assertEquals(originalDateTime, maskedDateTime);
  }

  @Test
  public void testTimeZone() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();

    configuration.setYearMask(false);
    configuration.setMonthMask(false);
    configuration.setDayMask(false);
    configuration.setHourMask(false);
    configuration.setMinutesMask(false);
    configuration.setSecondsMask(false);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String originalDateTime = "1981-08-08T00:00:00+04:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);
    System.out.println(maskedDateTime);
    assertTrue(maskedDateTime.equals("1981-08-07T20:00:00Z"));
  }

  @Ignore
  @Test
  public void testPerformance() {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();

    DateTimeMaskingProviderConfig fixedConfiguration = new DateTimeMaskingProviderConfig();
    fixedConfiguration.setFormatFixed("dd-MM-yyyy");

    DateTimeMaskingProviderConfig[] configurations = {maskingConfiguration, fixedConfiguration};

    for (DateTimeMaskingProviderConfig configuration : configurations) {
      DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

      int N = 1000000;
      String originalDateTime = "08-12-1981";

      long startMillis = System.currentTimeMillis();

      for (int i = 0; i < N; i++) {
        maskingProvider.mask(originalDateTime);
      }

      long diff = System.currentTimeMillis() - startMillis;
      System.out.println(String.format(" %d operations took %d milliseconds (%f per op)", N, diff,
          (double) diff / N));
      // Assert test always should finish in less than 30 seconds
      assertTrue(diff < 30000);
    }
  }

  @Test
  public void testGeneralizeWeekYear() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setGeneralizeWeekyear(true);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    // non-leap year
    assertEquals("01/2017", maskingProvider.mask("2017-01-01 00:00:00"));
    assertEquals("02/2017", maskingProvider.mask("2017/01/08 00:00:00"));
    assertEquals("52/2017", maskingProvider.mask("2017-12-30"));
    assertEquals("53/2017", maskingProvider.mask("2017-12-31"));

    // leap year
    assertEquals("01/2016", maskingProvider.mask("2016-01-05 00:00:00"));
    assertEquals("52/2016", maskingProvider.mask("2016-12-29"));
    assertEquals("53/2016", maskingProvider.mask("2016-12-30"));
    assertEquals("53/2016", maskingProvider.mask("2016-12-31"));
  }

  @Test
  public void testGeneralizeMonthYear() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setGeneralizeMonthyear(true);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String originalDateTime = "01-03-2016 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);
    System.out.println(maskedDateTime);
    assertTrue(maskedDateTime.equals("03/2016"));
  }

  @Test
  public void testAllDateFormats() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();

    configuration.setGeneralizeMonthyear(true);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String[] originalDateTime = new String[] {"2024-10-07T00:00:00Z", "2024-10-07", "07-10-2024",
        "2024/10/07", "07/10/2024", "2024-10-07 08:08:08", "07-10-2024 08:08:08",
        "2024/10/07 08:08:08", "07/10/2024 08:08:08"};
    for (String thisData : originalDateTime) {
      String maskedDateTime = maskingProvider.mask(thisData);
      System.out.println(maskedDateTime);
      assertTrue(maskedDateTime.equals("10/2024"));
    }
  }

  @Test
  public void testAllDateFormatsMidnight() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();

    configuration.setGeneralizeMonthyear(true);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String[] originalDateTime = new String[] {"2024-10-07T00:00:00Z", "2024-10-07", "07-10-2024",
        "2024/10/07", "07/10/2024", "2024-10-07 00:00:00", "07-10-2024 00:00:00",
        "2024/10/07 00:00:00", "07/10/2024 00:00:00"};
    for (String thisData : originalDateTime) {
      String maskedDateTime = maskingProvider.mask(thisData);
      System.out.println(maskedDateTime);
      assertTrue(maskedDateTime.equals("10/2024"));
    }
  }

  @Test
  public void testAllDateFormatsElevenFitty() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();

    configuration.setGeneralizeMonthyear(true);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String[] originalDateTime = new String[] {"2024-10-07T23:59:59Z", "2024-10-07", "07-10-2024",
        "2024/10/07", "07/10/2024", "2024-10-07 23:59:59", "07-10-2024 23:59:59",
        "2024/10/07 23:59:59", "07/10/2024 23:59:59"};
    for (String thisData : originalDateTime) {
      String maskedDateTime = maskingProvider.mask(thisData);
      System.out.println(maskedDateTime);
      assertTrue(maskedDateTime.equals("10/2024"));
    }
  }

  @Test
  public void testAllDateFormatsNoMask() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();

    configuration.setYearMask(false);
    configuration.setMonthMask(false);
    configuration.setDayMask(false);
    configuration.setHourMask(false);
    configuration.setMinutesMask(false);
    configuration.setSecondsMask(false);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String[] originalDateTime = new String[] {"2024-10-07T00:00:00Z", "2024-10-07", "07-10-2024",
        "2024/10/07", "07/10/2024", "2024-10-07 08:08:08", "07-10-2024 08:08:08",
        "2024/10/07 08:08:08", "07/10/2024 08:08:08"};
    for (String thisData : originalDateTime) {
      String maskedDateTime = maskingProvider.mask(thisData);
      System.out.println(maskedDateTime);
      assertTrue(maskedDateTime.equals(thisData));
    }
  }

  @Test
  public void testGeneralizeMonthYearISOStamp() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();

    configuration.setGeneralizeMonthyear(true);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String originalDateTime = "2014-10-07T14:45:00Z";
    String maskedDateTime = maskingProvider.mask(originalDateTime);
    System.out.println(maskedDateTime);
    assertTrue(maskedDateTime.equals("10/2014"));
  }

  @Test
  public void testGeneralizeQuarterYear() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setGeneralizeQuarteryear(true);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    assertEquals("01/2016", maskingProvider.mask("12-01-2016 00:00:00"));
    assertEquals("01/2016", maskingProvider.mask("2016/02/28"));
    assertEquals("01/2016", maskingProvider.mask("2016-03-18"));
    assertEquals("02/2016", maskingProvider.mask("12-04-2016"));
    String month = getMonthAbrvs()[4]; // May abbreviation in current locale
    assertEquals("02/2018", maskingProvider.mask("10-" + month + "-2018"));
    assertEquals("02/2017", maskingProvider.mask("12/06/2017"));
    assertEquals("03/2013", maskingProvider.mask("2013-07-04"));
    assertEquals("03/2015", maskingProvider.mask("2015-08-04T13:14:15-05:00"));
    assertEquals("03/2011", maskingProvider.mask("2011-09-03T10:15:30Z"));
    assertEquals("04/2010", maskingProvider.mask("2010-10-08 01:02:03"));
    assertEquals("04/2011", maskingProvider.mask("2011/11/08 15:02:03"));
    assertEquals("04/2016", maskingProvider.mask("12-12-2016 00:00:00"));
  }

  @Test
  public void testGeneralizeYear() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();

    configuration.setGeneralizeYear(true);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String originalDateTime = "12-12-2016 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);
    assertTrue(maskedDateTime.equals("2016"));
  }

  @Test
  public void testMaskFlagsFalse() {
    // By default shiftDate and all generalize options are false
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setYearMask(false);
    configuration.setMonthMask(false);
    configuration.setDayMask(false);
    configuration.setHourMask(false);
    configuration.setMinutesMask(false);
    configuration.setSecondsMask(false);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String originalDateTime = "19-05-2017 12:34:56";
    String maskedDateTime = maskingProvider.mask(originalDateTime);
    assertTrue(originalDateTime.equals(maskedDateTime));
  }

  @Test
  public void testMaskNullDateTimeInputReturnNull() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    MaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    String invalidDateTime = null;
    String maskedDateTime = maskingProvider.mask(invalidDateTime);

    assertEquals(null, maskedDateTime);
    assertTrue(outContent.toString().contains("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidDateTimeInputValidHandlingReturnNull() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    MaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    String invalidDateTime = "Invalid Date Time";
    String maskedDateTime = maskingProvider.mask(invalidDateTime);

    assertEquals(null, maskedDateTime);
    assertTrue(outContent.toString().contains("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidDateTimeInputValidHandlingReturnRandom() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    maskingConfiguration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    MaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);
    Identifier identifier = new DateTimeIdentifier();

    String invalidDateTime = "Invalid Date Time";
    String maskedDateTime = maskingProvider.mask(invalidDateTime);

    System.out.println(maskedDateTime + " " + invalidDateTime);

    assertFalse(maskedDateTime.equals(invalidDateTime));
    assertTrue(identifier.isOfThisType(maskedDateTime));
    assertTrue(outContent.toString().contains("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidDateTimeInputValidHandlingReturnDefaultCustomValue() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    maskingConfiguration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    MaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    String invalidDateTime = "Invalid Date Time";
    String maskedDateTime = maskingProvider.mask(invalidDateTime);

    assertEquals("OTHER", maskedDateTime);
    assertTrue(outContent.toString().contains("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidDateTimeInputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    maskingConfiguration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    maskingConfiguration.setUnexpectedInputReturnMessage("Test Date Time");
    MaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    String invalidDateTime = "Invalid Date Time";
    String maskedDateTime = maskingProvider.mask(invalidDateTime);

    assertEquals("Test Date Time", maskedDateTime);
    assertTrue(outContent.toString().contains("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidDateTimeInputInvalidHandlingReturnNull() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    maskingConfiguration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    MaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    assertNull(maskingProvider.mask("Invalid Date Time"));

    assertTrue(outContent.toString().contains("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskAgeOver90_GeneralizeYear() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setGeneralizeYearMaskAgeOver90(true);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String originalDateTime = "12-12-1900 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);
    LocalDateTime currentDate = LocalDateTime.now();
    Integer expectedMaskedYear = currentDate.minusYears(90).getYear();
    assertTrue(maskedDateTime.equals(expectedMaskedYear.toString()));
  }

  @Test
  public void testMaskAgeOver90_GeneralizeMonthYear() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setGeneralizeMonthyearMaskAgeOver90(true);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String originalDateTime = "12-12-1900 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);

    LocalDateTime currentDate = LocalDateTime.now();
    Integer expectedMaskedYear = currentDate.minusYears(90).getYear();
    Integer expectedMaskedMonth = currentDate.getMonthValue();
    assertTrue(maskedDateTime.contains(expectedMaskedMonth + "/" + expectedMaskedYear));
  }

  @Test
  public void testMaskAgeUnder90_GeneralizeMonthYear() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setGeneralizeMonthyearMaskAgeOver90(true);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String originalDateTime = "08-09-2010 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);
    assertTrue(maskedDateTime.equals("09/2010"));
  }

  @Test
  public void testMaskAgeEqual90_GeneralizeMonthYear() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setGeneralizeMonthyearMaskAgeOver90(true);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusYears(88);
    String originalDateTime =
        subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    String maskedDateTime = maskingProvider.mask(originalDateTime);

    String expectedDateTime = subtractedDate.format(DateTimeFormatter.ofPattern("MM/yyyy"));
    assertTrue(maskedDateTime.equals(expectedDateTime));
  }

  @Ignore("Ignore for now as this test fails on March 1.  To be fixed later")
  @Test
  public void testMaskAgeEqual90_GeneralizeMonthYear_WithOneDayOffset() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setGeneralizeMonthyearMaskAgeOver90(true);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusYears(89).minusDays(1);
    String originalDateTime =
        subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    String maskedDateTime = maskingProvider.mask(originalDateTime);

    Integer expectedMaskedYear = currentDate.minusYears(90).getYear();
    Integer expectedMaskedMonth = currentDate.getMonthValue();
    assertTrue(maskedDateTime.contains(expectedMaskedMonth + "/" + expectedMaskedYear));
  }

  @Test
  public void testMaskAgeOver90_GeneralizeMonthYear_LeapYear() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setGeneralizeMonthyearMaskAgeOver90(true);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String originalDateTime = "29-02-1920 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);

    LocalDateTime currentDate = LocalDateTime.now();
    Integer expectedMaskedYear = currentDate.minusYears(90).getYear();
    Integer expectedMaskedMonth = currentDate.getMonthValue();
    assertTrue(maskedDateTime.contains(expectedMaskedMonth + "/" + expectedMaskedYear));
  }

  @Test
  public void testDateYearDelete() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setYearDelete(true);
    configuration.setYearMask(false);
    configuration.setMonthMask(false);
    configuration.setDayMask(false);
    configuration.setHourMask(false);
    configuration.setMinutesMask(false);
    configuration.setSecondsMask(false);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    String originalDateTime = "12-12-2016 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);

    // Should remove year from date
    assertTrue(maskedDateTime.equals("12/12"));
  }

  @Test
  public void testDateYearDeleteBeforeNDays() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setYearDeleteNdays(true);
    configuration.setYearMask(false);
    configuration.setMonthMask(false);
    configuration.setDayMask(false);
    configuration.setHourMask(false);
    configuration.setMinutesMask(false);
    configuration.setSecondsMask(false);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusDays(375);
    String originalDateTime =
        subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    String maskedDateTime = maskingProvider.mask(originalDateTime);

    // Should return original date - input date is too long ago
    assertEquals(originalDateTime, maskedDateTime);
  }

  @Test
  public void testDateYearDeleteCustomizedNDays() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setYearDeleteNdays(true);
    configuration.setYearDeleteNdaysValue(800);
    configuration.setYearMask(false);
    configuration.setMonthMask(false);
    configuration.setDayMask(false);
    configuration.setHourMask(false);
    configuration.setMinutesMask(false);
    configuration.setSecondsMask(false);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusDays(790);
    String originalDateTime =
        subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    String expectedDateTime = subtractedDate.format(DateTimeFormatter.ofPattern("dd/MM"));
    String maskedDateTime = maskingProvider.mask(originalDateTime);

    // Should return masked date format, 790 days within 800 days
    assertTrue(maskedDateTime.equals(expectedDateTime));
  }

  @Test
  public void testMaskDayDefaultOptions() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setYearMask(false);
    configuration.setMonthMask(false);
    configuration.setDayMask(true);
    configuration.setHourMask(false);
    configuration.setMinutesMask(false);
    configuration.setSecondsMask(false);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    //
    // Default options; Within month
    //
    String originalDateTime = "19-05-2017 12:34:56";
    String startRange = "12-05-2017 12:34:56";
    String endRange = "19-05-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertTrue(isDateTimeInRange(maskedDateTime, startRange, endRange));
    }

    //
    // Default options; Span months
    //
    originalDateTime = "01-05-2017 12:34:56";
    startRange = "24-04-2017 12:34:56";
    endRange = "01-05-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertTrue(isDateTimeInRange(maskedDateTime, startRange, endRange));
    }
  }

  @Test
  public void testMaskDayDownwards() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setYearMask(false);
    configuration.setMonthMask(false);
    configuration.setDayMask(true);
    configuration.setHourMask(false);
    configuration.setMinutesMask(false);
    configuration.setSecondsMask(false);

    //
    // Specific number of days; Within month
    //
    configuration.setDayRangeUpMin(0);
    configuration.setDayRangeUp(0);
    configuration.setDayRangeDownMin(8);
    configuration.setDayRangeDown(8);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    String originalDateTime = "19-05-2017 12:34:56";
    String startRange = "11-05-2017 12:34:56";
    String endRange = "11-05-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertTrue(isDateTimeInRange(maskedDateTime, startRange, endRange));
    }

    //
    // Specific number of days; Span months
    //
    configuration.setDayRangeUpMin(0);
    configuration.setDayRangeUp(0);
    configuration.setDayRangeDownMin(5);
    configuration.setDayRangeDown(5);
    maskingProvider = new DateTimeMaskingProvider(configuration);

    originalDateTime = "01-05-2017 12:34:56";
    startRange = "26-04-2017 12:34:56";
    endRange = "26-04-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertTrue(isDateTimeInRange(maskedDateTime, startRange, endRange));
    }

    //
    // Ranged number of days; Within month
    //
    configuration.setDayRangeUpMin(0);
    configuration.setDayRangeUp(0);
    configuration.setDayRangeDownMin(9);
    configuration.setDayRangeDown(10);
    maskingProvider = new DateTimeMaskingProvider(configuration);

    originalDateTime = "19-05-2017 12:34:56";
    startRange = "09-05-2017 12:34:56";
    endRange = "10-05-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertTrue(isDateTimeInRange(maskedDateTime, startRange, endRange));
    }

    //
    // Ranged number of days; Span months
    //
    configuration.setDayRangeUpMin(0);
    configuration.setDayRangeUp(0);
    configuration.setDayRangeDownMin(14);
    configuration.setDayRangeDown(15);
    maskingProvider = new DateTimeMaskingProvider(configuration);

    originalDateTime = "01-05-2017 12:34:56";
    startRange = "16-04-2017 12:34:56";
    endRange = "17-04-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertTrue(isDateTimeInRange(maskedDateTime, startRange, endRange));
    }
  }

  @Test
  public void testMaskDayUpwards() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setYearMask(false);
    configuration.setMonthMask(false);
    configuration.setDayMask(true);
    configuration.setHourMask(false);
    configuration.setMinutesMask(false);
    configuration.setSecondsMask(false);

    //
    // Specific number of days; Within month
    //
    configuration.setDayRangeUpMin(8);
    configuration.setDayRangeUp(8);
    configuration.setDayRangeDownMin(0);
    configuration.setDayRangeDown(0);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    String originalDateTime = "19-05-2017 12:34:56";
    String startRange = "27-05-2017 12:34:56";
    String endRange = "27-05-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertTrue(isDateTimeInRange(maskedDateTime, startRange, endRange));
    }

    //
    // Specific number of days; Span months
    //
    configuration.setDayRangeUpMin(5);
    configuration.setDayRangeUp(5);
    configuration.setDayRangeDownMin(0);
    configuration.setDayRangeDown(0);
    maskingProvider = new DateTimeMaskingProvider(configuration);

    originalDateTime = "31-05-2017 12:34:56";
    startRange = "04-05-2017 12:34:56";
    endRange = "04-05-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertTrue(isDateTimeInRange(maskedDateTime, startRange, endRange));
    }

    //
    // Ranged number of days; Within month
    //
    configuration.setDayRangeUpMin(9);
    configuration.setDayRangeUp(10);
    configuration.setDayRangeDownMin(0);
    configuration.setDayRangeDown(0);
    maskingProvider = new DateTimeMaskingProvider(configuration);

    originalDateTime = "19-05-2017 12:34:56";
    startRange = "28-05-2017 12:34:56";
    endRange = "29-05-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertTrue(isDateTimeInRange(maskedDateTime, startRange, endRange));
    }

    //
    // Ranged number of days; Span months
    //
    configuration.setDayRangeUpMin(14);
    configuration.setDayRangeUp(15);
    configuration.setDayRangeDownMin(0);
    configuration.setDayRangeDown(0);
    maskingProvider = new DateTimeMaskingProvider(configuration);

    originalDateTime = "31-05-2017 12:34:56";
    startRange = "13-05-2017 12:34:56";
    endRange = "14-05-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertTrue(isDateTimeInRange(maskedDateTime, startRange, endRange));
    }
  }

  @Test
  public void testMaskDayDownwardsOrUpwards() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setYearMask(false);
    configuration.setMonthMask(false);
    configuration.setDayMask(true);
    configuration.setHourMask(false);
    configuration.setMinutesMask(false);
    configuration.setSecondsMask(false);

    //
    // Specific number of days; Within month
    //
    configuration.setDayRangeUpMin(8);
    configuration.setDayRangeUp(8);
    configuration.setDayRangeDownMin(4);
    configuration.setDayRangeDown(4);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    String originalDateTime = "19-05-2017 12:34:56";
    String startRangeDown = "15-05-2017 12:34:56";
    String endRangeDown = "15-05-2017 12:34:56";
    String startRangeUp = "27-05-2017 12:34:56";
    String endRangeUp = "27-05-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertTrue(isDateTimeInRange(maskedDateTime, startRangeDown, endRangeDown)
          || isDateTimeInRange(maskedDateTime, startRangeUp, endRangeUp));
    }

    //
    // Specific number of days; Span months
    //
    configuration.setDayRangeUpMin(20);
    configuration.setDayRangeUp(20);
    configuration.setDayRangeDownMin(25);
    configuration.setDayRangeDown(25);
    maskingProvider = new DateTimeMaskingProvider(configuration);

    originalDateTime = "19-05-2017 12:34:56";
    startRangeDown = "24-04-2017 12:34:56";
    endRangeDown = "24-04-2017 12:34:56";
    startRangeUp = "08-06-2017 12:34:56";
    endRangeUp = "08-06-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertTrue(isDateTimeInRange(maskedDateTime, startRangeDown, endRangeDown)
          || isDateTimeInRange(maskedDateTime, startRangeUp, endRangeUp));
    }

    //
    // Single ranged number of days
    //
    configuration.setDayRangeUpMin(0);
    configuration.setDayRangeUp(5);
    configuration.setDayRangeDownMin(0);
    configuration.setDayRangeDown(3);
    maskingProvider = new DateTimeMaskingProvider(configuration);

    originalDateTime = "19-05-2017 12:34:56";
    startRangeDown = "16-05-2017 12:34:56";
    endRangeUp = "24-05-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertTrue(isDateTimeInRange(maskedDateTime, startRangeDown, endRangeUp));
    }

    //
    // Multiple ranged number of days
    //
    configuration.setDayRangeUpMin(4);
    configuration.setDayRangeUp(5);
    configuration.setDayRangeDownMin(2);
    configuration.setDayRangeDown(3);
    maskingProvider = new DateTimeMaskingProvider(configuration);

    originalDateTime = "19-05-2017 12:34:56";
    startRangeDown = "16-05-2017 12:34:56";
    endRangeDown = "17-05-2017 12:34:56";
    startRangeUp = "23-05-2017 12:34:56";
    endRangeUp = "24-05-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertTrue(isDateTimeInRange(maskedDateTime, startRangeDown, endRangeDown)
          || isDateTimeInRange(maskedDateTime, startRangeUp, endRangeUp));
    }
  }

  /**
   * Determines if the specified datetime is contained (inclusive) in the specified datetime range.
   *
   * @param dateTime the datetime to check
   * @param startRange the start of the range, or null
   * @param endRange the end of the range, or null
   */
  private boolean isDateTimeInRange(String dateTime, String startRange, String endRange)
      throws ParseException {
    assertNotNull(dateTime);
    Date dateTimeAsDate = new SimpleDateFormat(DATE_TIME_FORMAT).parse(dateTime);
    assertNotNull(dateTimeAsDate);
    if (startRange != null) {
      Date startRangeAsDate = new SimpleDateFormat(DATE_TIME_FORMAT).parse(startRange);
      assertNotNull(startRangeAsDate);
      return !dateTimeAsDate.before(startRangeAsDate);
    }
    if (endRange != null) {
      Date endRangeAsDate = new SimpleDateFormat(DATE_TIME_FORMAT).parse(endRange);
      assertNotNull(endRangeAsDate);
      return !dateTimeAsDate.after(endRangeAsDate);
    }
    return true;
  }

  @Test
  public void testMaskMaxYears_OverMaxYears() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);

    maskingConfiguration.setYearMaxYearsAgoMask(true);
    maskingConfiguration.setYearMaxYearsAgo(90);
    maskingConfiguration.setYearShiftFromCurrentYear(52);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusYears(100);
    String originalDate = subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));

    String expectedDateTime =
        currentDate.minusYears(52).format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertEquals(expectedDateTime, maskedDateTime);
  }

  @Test
  public void testMaskMaxYears_OverMaxYears_leapyear() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);

    maskingConfiguration.setYearMaxYearsAgoMask(true);
    maskingConfiguration.setYearMaxYearsAgo(60);
    maskingConfiguration.setYearShiftFromCurrentYear(52);

    String originalDate = LocalDateTime.of(1908, 2, 29, 13, 14, 15)
        .format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    LocalDateTime currentDate = LocalDateTime.now();
    int targetyear = currentDate.get(ChronoField.YEAR) - 52;
    if (targetyear % 4 == 0 && (targetyear % 100 != 0 || targetyear % 400 == 0)) {
      // targetyear is also a leap year, go back one more year
      maskingConfiguration.setYearShiftFromCurrentYear(53);
    }

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    String maskedDateTime = maskingProvider.mask(originalDate);

    assertEquals("28-02-" + String.valueOf(targetyear) + " 13:14:15", maskedDateTime);
  }

  @Test
  public void testMaskMaxYears_OverMaxYears_ReturnOnlyYear() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);

    maskingConfiguration.setYearMaxYearsAgoMask(true);
    maskingConfiguration.setYearMaxYearsAgo(90);
    maskingConfiguration.setYearShiftFromCurrentYear(50);
    maskingConfiguration.setYearMaxYearsAgoOnlyYear(true);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusYears(100);
    String originalDate = subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));

    LocalDateTime expectedDateTime = currentDate.minusYears(50);
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertEquals(String.valueOf(expectedDateTime.getYear()), maskedDateTime);
  }

  @Test
  public void testMaskMaxYears_UnderMaxYears() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);

    maskingConfiguration.setYearMaxYearsAgoMask(true);
    maskingConfiguration.setYearMaxYearsAgo(90);
    maskingConfiguration.setYearShiftFromCurrentYear(50);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusYears(89);
    String originalDate = subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertEquals(originalDate, maskedDateTime);
  }

  @Test
  public void testMaskMaxYears_EqualMaxYears() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);

    maskingConfiguration.setYearMaxYearsAgoMask(true);
    maskingConfiguration.setYearMaxYearsAgo(92);
    maskingConfiguration.setYearShiftFromCurrentYear(50);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusYears(92);
    String originalDate = subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertEquals(originalDate, maskedDateTime);
  }

  @Test
  public void testMaskMaxYears_EqualMaxYears_PlusDays() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);

    maskingConfiguration.setYearMaxYearsAgoMask(true);
    maskingConfiguration.setYearMaxYearsAgo(92);
    maskingConfiguration.setYearShiftFromCurrentYear(50);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusYears(92).minusDays(300);
    String originalDate = subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertEquals(originalDate, maskedDateTime);
  }

  @Test
  public void testMaskMaxYears_NotEnabled() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);

    maskingConfiguration.setYearMaxYearsAgoMask(false);
    maskingConfiguration.setYearMaxYearsAgo(90);
    maskingConfiguration.setYearShiftFromCurrentYear(50);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    String originalDate = "02-12-1924 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertEquals(originalDate, maskedDateTime);
    assertEquals("1924", getYear(maskedDateTime));
  }

  @Test
  public void testMaskMaxDays_OverMaxYears() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);

    maskingConfiguration.setDayMaxDaysAgoMask(true);
    maskingConfiguration.setDayMaxDaysAgo(32850);
    maskingConfiguration.setDayShiftFromCurrentDay(18250);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusDays(33150);
    String originalDate = subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));

    Integer expectedMaskedYear = currentDate.minusDays(18250).getYear();
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertFalse(maskedDateTime.equals(originalDate));
    assertEquals(expectedMaskedYear.toString(), getYear(maskedDateTime));
  }

  @Test
  public void testMaskMaxDays_OverMaxYears_ReturnOnlyYear() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);

    maskingConfiguration.setDayMaxDaysAgoMask(true);
    maskingConfiguration.setDayMaxDaysAgo(32850);
    maskingConfiguration.setYearMaxYearsAgoOnlyYear(true);
    maskingConfiguration.setDayShiftFromCurrentDay(18250);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusDays(33150);
    String originalDate = subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));

    Integer expectedMaskedYear = currentDate.minusDays(18250).getYear();
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertFalse(maskedDateTime.equals(originalDate));
    assertEquals(expectedMaskedYear.toString(), maskedDateTime);
  }

  @Test
  public void testMaskMaxDays_UnderMaxYears() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);

    maskingConfiguration.setDayMaxDaysAgoMask(true);
    maskingConfiguration.setDayMaxDaysAgo(32850);
    maskingConfiguration.setDayShiftFromCurrentDay(18250);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusDays(30850);
    String originalDate = subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertTrue(maskedDateTime.equals(originalDate));
  }

  @Test
  public void testMaskMaxDays_NotEnabled() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);

    maskingConfiguration.setDayMaxDaysAgoMask(false);
    maskingConfiguration.setDayMaxDaysAgo(32850);
    maskingConfiguration.setDayShiftFromCurrentDay(18250);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    String originalDate = "02-12-1924 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertTrue(originalDate.equals(maskedDateTime));
    assertEquals("1924", getYear(maskedDateTime));
  }

  @Test
  // jra
  public void testMaskOverride_NotEnabled() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);

    maskingConfiguration.setOverrideMask(false);
    maskingConfiguration.setOverrideYearsPassed(90);
    maskingConfiguration.setOverrideValue("done anyway");
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    String originalDate = "1910-02-12 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertTrue(maskedDateTime.equals(originalDate));
  }

  @Test
  public void testMaskOverride_UnderYearsPassed() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);

    maskingConfiguration.setOverrideMask(true);
    maskingConfiguration.setOverrideYearsPassed(90);
    maskingConfiguration.setOverrideValue(null);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusYears(70);
    String originalDate = subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertTrue(originalDate.equals(maskedDateTime));
  }

  @Test
  public void testMaskOverride_OverYearsPassed() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);

    maskingConfiguration.setOverrideMask(true);
    maskingConfiguration.setOverrideYearsPassed(90);
    maskingConfiguration.setOverrideValue(null);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    // The date time should be overridden using <yearsPassed>+
    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusYears(100);
    String originalDate = subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertTrue("90+".equals(maskedDateTime));
  }

  @Test
  public void testMaskOverride_OverYearsPassed_LeapYear() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);

    maskingConfiguration.setOverrideMask(true);
    maskingConfiguration.setOverrideYearsPassed(80);
    maskingConfiguration.setOverrideValue(null);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    // The date time should be overridden using <yearsPassed>+
    String originalDate = "1920-02-29 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertTrue("80+".equals(maskedDateTime));
  }

  @Test
  public void testMaskOverride_OverYearsPassed_WithOverrideValue() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);

    maskingConfiguration.setOverrideMask(true);
    maskingConfiguration.setOverrideYearsPassed(70);
    maskingConfiguration.setOverrideValue("OVER 70");
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusYears(90);
    String originalDate = subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertTrue("OVER 70".equals(maskedDateTime));
  }

  @Test
  public void testYearMask() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setYearMask(true);
    configuration.setYearRangeDown(5);
    configuration.setYearRangeUp(10);
    maskComponent("21-12-2016 18:46:33", "21-12-xx 18:46:33", ChronoField.YEAR, 2016, 2011, 2026,
        true, configuration);
  }

  @Test
  public void testMonthMask() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setMonthMask(true);
    configuration.setMonthRangeDown(4);
    configuration.setMonthRangeUp(5);
    maskComponent("21-05-2016 18:46:33", "21-xx-2016 18:46:33", ChronoField.MONTH_OF_YEAR, 5, 1, 10,
        true, configuration);
  }

  @Test
  public void testDayMask() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setDayMask(true);
    configuration.setDayRangeDown(19);
    configuration.setDayRangeDownMin(1);
    configuration.setDayRangeUp(10);
    configuration.setDayRangeUpMin(1);
    maskComponent("21-05-2016 18:46:33", "xx-05-2016 18:46:33", ChronoField.DAY_OF_MONTH, 21, 2, 31,
        false, configuration);
  }

  @Test
  public void testHourMask() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setHourMask(true);
    configuration.setHourRangeDown(10);
    configuration.setHourRangeUp(5);
    maskComponent("21-05-2016 18:46:33", "21-05-2016 xx:46:33", ChronoField.HOUR_OF_DAY, 18, 8, 23,
        true, configuration);
  }

  @Test
  public void testMinuteMask() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setMinutesMask(true);
    configuration.setMinutesRangeDown(30);
    configuration.setMinutesRangeUp(12);
    maskComponent("21-05-2016 18:46:33", "21-05-2016 18:xx:33", ChronoField.MINUTE_OF_HOUR, 46, 16,
        58, true, configuration);
  }

  @Test
  public void testSecondMask() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setSecondsMask(true);
    configuration.setSecondsRangeDown(20);
    configuration.setSecondsRangeUp(15);
    maskComponent("21-05-2016 18:46:33", "21-05-2016 18:46:xx", ChronoField.SECOND_OF_MINUTE, 33,
        13, 48, true, configuration);
  }

  private void maskComponent(String originalDateTime, String pattern, TemporalField fieldenum,
      int original, int min, int max, boolean originalOK,
      DateTimeMaskingProviderConfig configuration)
      throws Exception {
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    TreeMap<Integer, Integer> map = new TreeMap<>();
    for (int idx = 0; idx < 100; idx++) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      LocalDateTime masked = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss")
          .parse(maskedDateTime).query(LocalDateTime::from);
      Integer field = Integer.valueOf(masked.get(fieldenum));
      assertTrue("field = " + field, field.intValue() >= min && field.intValue() <= max);
      assertTrue(originalOK || field.intValue() != original);
      String expected = pattern.replace("xx", String.format("%02d", field));
      assertEquals(expected, maskedDateTime);
      Integer count = map.get(field);
      if (count == null) {
        map.put(field, Integer.valueOf(1));
      } else {
        map.put(field, Integer.valueOf(count.intValue() + 1));
      }
    }
    System.out.println(map);
    Integer count = map.get(Integer.valueOf(original));
    // verify value is actually changing - random replacement shouldn't select original value 80
    // times out of a 100
    assertTrue(count == null || count.intValue() < 80);
  }

  private void setAllDateTimeMaskingToFalse(DateTimeMaskingProviderConfig configuration) {
    configuration.setGeneralizeWeekyear(false);
    configuration.setGeneralizeMonthyear(false);
    configuration.setGeneralizeQuarteryear(false);
    configuration.setGeneralizeYear(false);
    configuration.setYearMask(false);
    configuration.setMonthMask(false);
    configuration.setDayMask(false);
    configuration.setHourMask(false);
    configuration.setMinutesMask(false);
    configuration.setSecondsMask(false);
    configuration.setMaskShiftDate(false);
    configuration.setOverrideMask(false);
  }

  private String getYear(String originalDate) {
    return StringUtils.substring(originalDate, 6, 10);
  }

  /**
   * Gets the month abbreviations in the default locale, which can be different for each caller.
   */
  private String[] getMonthAbrvs() {
    String[] abrvs = new String[12];
    DateTimeFormatter f = new DateTimeFormatterBuilder().appendPattern("MMM").toFormatter();
    for (int i = 0; i < 12; i++) {
      abrvs[i] = f.format(LocalDate.of(2022, i + 1, 20));
    }
    return abrvs;
  }
}
