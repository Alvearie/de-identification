/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.time.temporal.TemporalField;
import java.time.temporal.UnsupportedTemporalTypeException;
import java.util.TreeMap;
import org.apache.commons.lang.StringUtils;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.DateTimeIdentifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
import com.ibm.whc.deid.utils.log.LogCodes;

public class DateTimeMaskingProviderTest extends TestLogSetUp {
  private static final int NUM_LOOP_NON_PERF_TEST = 20;

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
    config.setMaskShiftSeconds(-121);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(config);

    assertEquals("1981-12-13T13:02:00-05:00", maskingProvider.mask("1981-12-13T13:04:01-05:00"));
  }

  @Test
  public void testMaskFixedFormat() throws Exception {

    DateTimeMaskingProviderConfig config = new DateTimeMaskingProviderConfig();
    config.setFormatFixed("MM-dd-yyyy");
    config.setYearMask(false);
    config.setDayMask(false);
    config.setHourMask(false);
    config.setMinuteMask(false);
    config.setSecondMask(false);
    config.setMonthMask(true);
    config.setMonthRangeDown(0);
    config.setMonthRangeUp(2);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(config);

    String originalDateTime = "08-22-1981"; // 00:00:00
    boolean changed = false;
    for (int i = 0; i < 10; i++) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertTrue("08-22-1981".equals(maskedDateTime) || "09-22-1981".equals(maskedDateTime)
          || "10-22-1981".equals(maskedDateTime));
      if (!maskedDateTime.equals(originalDateTime)) {
        changed = true;
      }
    }
    assertTrue(changed);
  }

  @Test
  public void testMaskFixedFormat_not_enough_data() throws Exception {

    DateTimeMaskingProviderConfig config = new DateTimeMaskingProviderConfig();
    config.setFormatFixed("yyyy");
    config.setYearMask(true);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(config);

    try {
      maskingProvider.mask("1981");
      fail("expected exception");
    } catch (IllegalArgumentException e) {
      System.out.println(e.getMessage());
      assertTrue(e.getMessage().startsWith(LogCodes.WPH1025W));
      assertTrue(e.getMessage().contains("`yyyy`"));
    }

    config.setFormatFixed("yyyy-MM");
    maskingProvider = new DateTimeMaskingProvider(config);

    try {
      maskingProvider.mask("1981-04");
      fail("expected exception");
    } catch (IllegalArgumentException e) {
      System.out.println(e.getMessage());
      assertTrue(e.getMessage().startsWith(LogCodes.WPH1025W));
      assertTrue(e.getMessage().contains("`yyyy-MM`"));
    }
  }

  @Test
  public void testMaskFixedFormat_badInput() throws Exception {

    DateTimeMaskingProviderConfig config = new DateTimeMaskingProviderConfig();
    config.setFormatFixed("MM-dd-yyyy");
    config.setYearMask(false);
    config.setDayMask(false);
    config.setHourMask(false);
    config.setMinuteMask(false);
    config.setSecondMask(false);
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
    config.setMinuteMask(false);
    config.setSecondMask(false);
    config.setMonthMask(true);
    config.setMonthRangeDown(0);
    config.setMonthRangeUp(2);
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(config);

    String originalDateTime = "18-22-1981";
    for (int i = 0; i < 100; i++) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      assertNotNull(maskedDateTime);
      assertEquals(originalDateTime.length(), maskedDateTime.length());
      int month = Integer.parseInt(maskedDateTime.substring(0, 2));
      assertTrue(Integer.toString(month), month >= 1 && month <= 12);
      assertEquals("-", maskedDateTime.substring(2, 3));
    }
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
  public void testGeneralizeMonthYear_allDateFormats() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setGeneralizeMonthyear(true);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String[] months = getMonthAbbreviations();

    String[] originalDateTime = new String[] {"2024-10-07T00:00:00Z", "2024-10-07", "07-10-2024",
        "2024/10/07", "07/10/2024", "2024-10-07 08:08:08", "07-10-2024 08:08:08",
        "2024/10/07 08:08:08", "07/10/2024 08:08:08", "07-" + months[9] + "-2024"};
    for (String thisData : originalDateTime) {
      String maskedDateTime = maskingProvider.mask(thisData);
      assertEquals("10/2024", maskedDateTime);
    }
  }

  @Test
  public void testGeneralizeMonthYear_outputFormat() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setGeneralizeMonthyear(true);
    configuration.setGeneralizeMonthYearOutputFormat("yyyy-MMM");
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String[] months = getMonthAbbreviations();

    String[] originalDateTime = new String[] {"2024-10-07T00:00:00Z", "2024-10-07", "07-10-2024",
        "2024/10/07", "07/10/2024", "2024-10-07 08:08:08", "07-10-2024 08:08:08",
        "2024/10/07 08:08:08", "07/10/2024 08:08:08", "07-" + months[9] + "-2024"};
    for (String thisData : originalDateTime) {
      String maskedDateTime = maskingProvider.mask(thisData);
      assertEquals("2024-" + months[9], maskedDateTime);
    }
  }

  @Test
  public void testGeneralizeMonthYear_testAllDateFormatsMidnight() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setGeneralizeMonthyear(true);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String[] months = getMonthAbbreviations();

    String[] originalDateTime = new String[] {"2024-10-07T00:00:00Z", "2024-10-07", "07-10-2024",
        "2024/10/07", "07/10/2024", "2024-10-07 00:00:00", "07-10-2024 00:00:00",
        "2024/10/07 00:00:00", "07/10/2024 00:00:00", "07-" + months[9] + "-2024"};
    for (String thisData : originalDateTime) {
      String maskedDateTime = maskingProvider.mask(thisData);
      assertEquals("10/2024", maskedDateTime);
    }
  }

  @Test
  public void testGeneralizeMonthYear_testAllDateFormatsLate() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setGeneralizeMonthyear(true);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String[] months = getMonthAbbreviations();

    String[] originalDateTime = new String[] {"2024-10-07T23:59:59Z", "2024-10-07", "07-10-2024",
        "2024/10/07", "07/10/2024", "2024-10-07 23:59:59", "07-10-2024 23:59:59",
        "2024/10/07 23:59:59", "07/10/2024 23:59:59", "07-" + months[9] + "-2024"};
    for (String thisData : originalDateTime) {
      String maskedDateTime = maskingProvider.mask(thisData);
      assertEquals("failure for " + thisData, "10/2024", maskedDateTime);
    }
  }

  @Test
  public void testAllDateFormatsNoMask() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String[] months = getMonthAbbreviations();

    String[] originalDateTime = new String[] {"2024-10-07T00:00:00Z", "2024-10-07", "07-10-2024",
        "2024/10/07", "07/10/2024", "2024-10-07 08:08:08", "07-10-2024 08:08:08",
        "2024/10/07 08:08:08", "07/10/2024 08:08:08", "07-" + months[9] + "-2024"};
    for (String thisData : originalDateTime) {
      String maskedDateTime = maskingProvider.mask(thisData);
      assertEquals(thisData, maskedDateTime);
    }
  }

  @Test
  public void testGeneralizeQuarterYear() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setGeneralizeQuarteryear(true);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    assertEquals("1/2016", maskingProvider.mask("12-01-2016 00:00:00"));
    assertEquals("1/2016", maskingProvider.mask("2016/02/28"));
    assertEquals("1/2016", maskingProvider.mask("2016-03-18"));
    assertEquals("2/2016", maskingProvider.mask("12-04-2016"));
    String month = getMonthAbbreviations()[4]; // May abbreviation in current locale
    assertEquals("2/2018", maskingProvider.mask("10-" + month + "-2018"));
    assertEquals("2/2017", maskingProvider.mask("12/06/2017"));
    assertEquals("3/2013", maskingProvider.mask("2013-07-04"));
    assertEquals("3/2015", maskingProvider.mask("2015-08-04T13:14:15-05:00"));
    assertEquals("3/2011", maskingProvider.mask("2011-09-03T10:15:30Z"));
    assertEquals("4/2010", maskingProvider.mask("2010-10-08 01:02:03"));
    assertEquals("4/2011", maskingProvider.mask("2011/11/08 15:02:03"));
    assertEquals("4/2016", maskingProvider.mask("12-12-2016 00:00:00"));
  }

  @Test
  public void testGeneralizeQuarterYear_outputFormat() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setGeneralizeQuarteryear(true);
    configuration.setGeneralizeQuarterYearOutputFormat("Q'Q'yyyy");
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    assertEquals("1Q2016", maskingProvider.mask("12-01-2016 00:00:00"));
    assertEquals("1Q2016", maskingProvider.mask("2016/02/28"));
    assertEquals("1Q2016", maskingProvider.mask("2016-03-18"));
    assertEquals("2Q2016", maskingProvider.mask("12-04-2016"));
    String month = getMonthAbbreviations()[4]; // May abbreviation in current locale
    assertEquals("2Q2018", maskingProvider.mask("10-" + month + "-2018"));
    assertEquals("2Q2017", maskingProvider.mask("12/06/2017"));
    assertEquals("3Q2013", maskingProvider.mask("2013-07-04"));
    assertEquals("3Q2015", maskingProvider.mask("2015-08-04T13:14:15-05:00"));
    assertEquals("3Q2011", maskingProvider.mask("2011-09-03T10:15:30Z"));
    assertEquals("4Q2010", maskingProvider.mask("2010-10-08 01:02:03"));
    assertEquals("4Q2011", maskingProvider.mask("2011/11/08 15:02:03"));
    assertEquals("4Q2016", maskingProvider.mask("12-12-2016 00:00:00"));
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
    configuration.setMinuteMask(false);
    configuration.setSecondMask(false);

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
  public void testGeneralizeYearMaskAgeOver90_Over90() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setGeneralizeYearMaskAgeOver90(true);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    String originalDateTime = "12-12-1900 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);
    int expectedYear = LocalDateTime.now().minusYears(90).getYear();
    assertEquals(String.valueOf(expectedYear), maskedDateTime);
  }

  @Test
  public void testGeneralizeMonthyearMaskAgeOver90_Over90() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setGeneralizeMonthyearMaskAgeOver90(true);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    String originalDateTime = "12-12-1900 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);

    LocalDateTime currentDate = LocalDateTime.now();
    int expectedMaskedYear = currentDate.minusYears(90).getYear();
    int expectedMaskedMonth = 12;

    assertEquals(String.format("%02d/%d", expectedMaskedMonth, expectedMaskedYear), maskedDateTime);

    // override output format
    configuration.setGeneralizeMonthYearMaskAgeOver90OutputFormat("yyyy--MM");
    maskingProvider = new DateTimeMaskingProvider(configuration);

    maskedDateTime = maskingProvider.mask(originalDateTime);

    assertEquals(String.format("%d--%02d", expectedMaskedYear, expectedMaskedMonth),
        maskedDateTime);
  }

  @Test
  public void testGeneralizeMonthyearMaskAgeOver90_Under90() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setGeneralizeMonthyearMaskAgeOver90(true);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);
    
    String originalDateTime = "08-09-2010 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);
    
    assertEquals("09/2010", maskedDateTime);
    
    // override output format
    configuration.setGeneralizeMonthYearMaskAgeOver90OutputFormat("yyyy'X'MM");
    maskingProvider = new DateTimeMaskingProvider(configuration);

    maskedDateTime = maskingProvider.mask(originalDateTime);

    assertEquals("2010X09", maskedDateTime);
  }

  @Test
  public void testGeneralizeMonthyearMaskAgeOver90_Equal90() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setGeneralizeMonthyearMaskAgeOver90(true);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusYears(90).plusMonths(1);
    String originalDateTime =
        subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    String maskedDateTime = maskingProvider.mask(originalDateTime);

    assertEquals(String.format("%02d/%d", subtractedDate.getMonthValue(), subtractedDate.getYear()),
        maskedDateTime);
  }

  @Test
  public void testGeneralizeMonthyearMaskAge_OneDayShort() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setGeneralizeMonthyearMaskAgeOver90(true);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusYears(90).plusDays(1);
    String originalDateTime =
        subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    String maskedDateTime = maskingProvider.mask(originalDateTime);

    assertEquals(String.format("%02d/%d", subtractedDate.getMonthValue(), subtractedDate.getYear()),
        maskedDateTime);
  }

  @Test
  public void testDateYearDelete() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setYearDelete(true);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    String originalDateTime = "13-04-2016 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDateTime);

    assertEquals("13/04", maskedDateTime);

    // output format override
    configuration.setYearDeleteOutputFormat("MMM-dd");
    maskingProvider = new DateTimeMaskingProvider(configuration);

    maskedDateTime = maskingProvider.mask(originalDateTime);

    assertEquals(getMonthAbbreviations()[3] + "-13", maskedDateTime);
  }

  @Test
  public void testYearDeleteNDays_before() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setYearDeleteNdays(true);
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
  public void testYearDeleteNDays_custom_after() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setYearDeleteNdays(true);
    configuration.setYearDeleteNdaysValue(800);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusDays(790);
    String originalDateTime =
        subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    String expectedDateTime = subtractedDate.format(DateTimeFormatter.ofPattern("dd/MM"));
    String maskedDateTime = maskingProvider.mask(originalDateTime);

    // Should return masked date format, 790 days within 800 days
    assertEquals(expectedDateTime, maskedDateTime);
  }

  @Test
  public void testYearDeleteNDays_pattern_bad() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setYearDeleteNdays(true);
    // unexpected format - too many values
    configuration.setYearDeleteNdaysOutputFormat("yyyy-MM-dd'T'HH:mm:ssZ");
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusDays(30);
    String originalDateTime =
        subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    try {
      maskingProvider.mask(originalDateTime);
    } catch (UnsupportedTemporalTypeException e) {
      // good
      System.out.println(e.getMessage());
    }

    configuration.setYearDeleteNdaysOutputFormat("MM-ddZ");
    maskingProvider = new DateTimeMaskingProvider(configuration);
    try {
      maskingProvider.mask(originalDateTime);
    } catch (UnsupportedTemporalTypeException e) {
      // good
      System.out.println(e.getMessage());
    }
  }

  @Test
  public void testYearDeleteNDays_pattern() {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setYearDeleteNdays(true);
    // unexpected format - too many values
    configuration.setYearDeleteNdaysOutputFormat("'Month='MM'Day='dd");
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusDays(30);
    String originalDateTime =
        subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));
    String expectedDateTime =
        String.format("Month=%02dDay=%02d", subtractedDate.getMonthValue(),
            subtractedDate.getDayOfMonth());

    String maskedDateTime = maskingProvider.mask(originalDateTime);

    // Should return masked date format, 790 days within 800 days
    assertEquals(expectedDateTime, maskedDateTime);
  }

  @Test
  public void testMaskDayDefaultOptions() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setYearMask(false);
    configuration.setMonthMask(false);
    configuration.setDayMask(true);
    configuration.setHourMask(false);
    configuration.setMinuteMask(false);
    configuration.setSecondMask(false);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    //
    // Default options; Within month
    //
    String originalDateTime = "19-05-2017 12:34:56";
    String startRange = "12-05-2017 12:34:56";
    String endRange = "19-05-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      isDateTimeInRange(maskedDateTime, startRange, endRange, "dd-MM-yyyy HH:mm:ss");
    }

    //
    // Default options; Span months
    //
    originalDateTime = "01-05-2017 12:34:56";
    startRange = "24-04-2017 12:34:56";
    endRange = "01-05-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      isDateTimeInRange(maskedDateTime, startRange, endRange, "dd-MM-yyyy HH:mm:ss");
    }
  }

  @Test
  public void testMaskDayDownwards() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setYearMask(false);
    configuration.setMonthMask(false);
    configuration.setDayMask(true);
    configuration.setHourMask(false);
    configuration.setMinuteMask(false);
    configuration.setSecondMask(false);

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
      isDateTimeInRange(maskedDateTime, startRange, endRange, "dd-MM-yyyy HH:mm:ss");
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
      isDateTimeInRange(maskedDateTime, startRange, endRange, "dd-MM-yyyy HH:mm:ss");
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
      isDateTimeInRange(maskedDateTime, startRange, endRange, "dd-MM-yyyy HH:mm:ss");
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
      isDateTimeInRange(maskedDateTime, startRange, endRange, "dd-MM-yyyy HH:mm:ss");
    }
  }

  @Test
  public void testMaskDayUpwards() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setYearMask(false);
    configuration.setMonthMask(false);
    configuration.setDayMask(true);
    configuration.setHourMask(false);
    configuration.setMinuteMask(false);
    configuration.setSecondMask(false);

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
      isDateTimeInRange(maskedDateTime, startRange, endRange, "dd-MM-yyyy HH:mm:ss");
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
    startRange = "05-06-2017 12:34:56";
    endRange = "05-06-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      isDateTimeInRange(maskedDateTime, startRange, endRange, "dd-MM-yyyy HH:mm:ss");
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
      isDateTimeInRange(maskedDateTime, startRange, endRange, "dd-MM-yyyy HH:mm:ss");
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
    startRange = "14-06-2017 12:34:56";
    endRange = "15-06-2017 12:34:56";

    for (int i = 0; i < NUM_LOOP_NON_PERF_TEST; ++i) {
      String maskedDateTime = maskingProvider.mask(originalDateTime);
      isDateTimeInRange(maskedDateTime, startRange, endRange, "dd-MM-yyyy HH:mm:ss");
    }
  }

  @Test
  public void testMaskDayDownwardsOrUpwards() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    configuration.setYearMask(false);
    configuration.setMonthMask(false);
    configuration.setDayMask(true);
    configuration.setHourMask(false);
    configuration.setMinuteMask(false);
    configuration.setSecondMask(false);

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
      try {
        isDateTimeInRange(maskedDateTime, startRangeDown, endRangeDown, "dd-MM-yyyy HH:mm:ss");
      } catch (RangeException e) {
        isDateTimeInRange(maskedDateTime, startRangeUp, endRangeUp, "dd-MM-yyyy HH:mm:ss");
      }
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
      try {
        isDateTimeInRange(maskedDateTime, startRangeDown, endRangeDown, "dd-MM-yyyy HH:mm:ss");
      } catch (RangeException e) {
        isDateTimeInRange(maskedDateTime, startRangeUp, endRangeUp, "dd-MM-yyyy HH:mm:ss");
      }
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
      isDateTimeInRange(maskedDateTime, startRangeDown, endRangeUp, "dd-MM-yyyy HH:mm:ss");
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
      try {
        isDateTimeInRange(maskedDateTime, startRangeDown, endRangeDown, "dd-MM-yyyy HH:mm:ss");
      } catch (RangeException e) {
        isDateTimeInRange(maskedDateTime, startRangeUp, endRangeUp, "dd-MM-yyyy HH:mm:ss");
      }
    }
  }

  private class RangeException extends Exception {

    private static final long serialVersionUID = 1L;

    public RangeException(String message) {
      super(message);
    }
  }
  
  /**
   * Determines if the specified datetime is contained (inclusive) in the specified datetime range.
   *
   * @param dateTime the datetime to check
   * @param startRange the start of the range, or null
   * @param endRange the end of the range, or null
   * 
   * @throws RangeException if the date is out of range
   */
  private void isDateTimeInRange(String dateTime, String startRange, String endRange,
      String formatPattern) throws RangeException {
    DateTimeFormatter f = new DateTimeFormatterBuilder().parseCaseInsensitive()
        .appendPattern(formatPattern).parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
        .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
        .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0).toFormatter();

    if (dateTime == null) {
      throw new RangeException("datetime is null");
    }
    LocalDateTime dateTimeAsDate = LocalDateTime.from(f.parse(dateTime));
    assertNotNull(dateTimeAsDate);
    if (startRange != null) {
      LocalDateTime startRangeAsDate = LocalDateTime.from(f.parse(startRange));
      if (dateTimeAsDate.isBefore(startRangeAsDate)) {
        throw new RangeException(dateTime + " is before " + startRange);
      }
    }
    if (endRange != null) {
      LocalDateTime endRangeAsDate = LocalDateTime.from(f.parse(endRange));
      if (dateTimeAsDate.isAfter(endRangeAsDate)) {
        throw new RangeException(dateTime + " is after " + endRange);
      }
    }
  }

  @Test
  public void testMaskMaxYears_OverMaxYears() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);
    maskingConfiguration.setYearMaxYearsAgoMask(true);
    maskingConfiguration.setYearMaxYearsAgo(90);
    maskingConfiguration.setYearShiftFromCurrentYear(52);

    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    String originalDate = "16-04-1922 13:14:15";
    int expectedYear = LocalDateTime.now().minusYears(52).getYear();
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertEquals(
        originalDate.substring(0, 6) + String.valueOf(expectedYear) + originalDate.substring(10),
        maskedDateTime);

    // check capitalization
    String[] monthNames = getMonthAbbreviations();
    String name = monthNames[1];
    originalDate = "16-" + name + "-1921";
    maskedDateTime = maskingProvider.mask(originalDate);
    assertEquals(String.format("%02d-%s-%d", 16, name, expectedYear), maskedDateTime);

    maskedDateTime = maskingProvider.mask(originalDate.toUpperCase());
    assertEquals(String.format("%02d-%s-%d", 16, name.toUpperCase(), expectedYear), maskedDateTime);

    maskedDateTime = maskingProvider.mask(originalDate.toLowerCase());
    assertEquals(String.format("%02d-%s-%d", 16, name.toLowerCase(), expectedYear), maskedDateTime);
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

    int expectedYear = currentDate.minusYears(50).getYear();

    String maskedDateTime = maskingProvider.mask(originalDate);

    assertEquals(
        originalDate.substring(0, 6) + String.valueOf(expectedYear) + originalDate.substring(10),
        maskedDateTime);
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
    LocalDateTime subtractedDate = currentDate.minusYears(92).plusDays(1);
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
  public void testDayMaxDaysAgoMask_Over() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);
    maskingConfiguration.setDayMaxDaysAgoMask(true);
    maskingConfiguration.setDayMaxDaysAgo(32850);
    maskingConfiguration.setDayShiftFromCurrentDay(18250);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusDays(32851L);
    int expectedMon = subtractedDate.getMonthValue();
    int expectedDay = subtractedDate.getDayOfMonth();
    // if the expected date is Feb 29, changing the year might change the date -
    // use one more day ago to avoid overly complicating the test
    if (expectedMon == 2 && expectedDay == 29) {
      subtractedDate = subtractedDate.minusDays(1L);
      expectedDay = 28;
    }
    int expectedHour = subtractedDate.getHour();
    int expectedMin = subtractedDate.getMinute();
    int expectedSec = subtractedDate.getSecond();
    String originalDate = subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));

    int expectedYear = currentDate.minusDays(18250).getYear();

    String maskedDateTime = maskingProvider.mask(originalDate);

    assertEquals(String.format("%02d-%02d-%d %02d:%02d:%02d", expectedDay, expectedMon,
        expectedYear, expectedHour, expectedMin, expectedSec), maskedDateTime);

    // check capitalization
    originalDate = subtractedDate.format(DateTimeFormatter.ofPattern("dd-MMM-yyyy"));
    maskedDateTime = maskingProvider.mask(originalDate);
    String[] monthNames = getMonthAbbreviations();
    String name = monthNames[expectedMon - 1];
    assertEquals(String.format("%02d-%s-%d", expectedDay, name, expectedYear), maskedDateTime);

    maskedDateTime = maskingProvider.mask(originalDate.toUpperCase());
    assertEquals(String.format("%02d-%s-%d", expectedDay, name.toUpperCase(), expectedYear),
        maskedDateTime);

    maskedDateTime = maskingProvider.mask(originalDate.toLowerCase());
    assertEquals(String.format("%02d-%s-%d", expectedDay, name.toLowerCase(), expectedYear),
        maskedDateTime);
  }

  @Test
  public void testDayMaxDaysAgoMask_Over_ReturnOnlyYear() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);
    maskingConfiguration.setDayMaxDaysAgoMask(true);
    maskingConfiguration.setDayMaxDaysAgo(32850);
    maskingConfiguration.setDayMaxDaysAgoOnlyYear(true);
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
  public void testDayMaxDaysAgoMask_Equal() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);
    maskingConfiguration.setDayMaxDaysAgoMask(true);
    maskingConfiguration.setDayMaxDaysAgo(32850);
    maskingConfiguration.setDayShiftFromCurrentDay(18250);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime subtractedDate = currentDate.minusDays(32850L);
    int expectedMon = subtractedDate.getMonthValue();
    int expectedDay = subtractedDate.getDayOfMonth();
    // if the expected date is Feb 29, changing the year might change the date
    boolean expectedLeapDay = expectedMon == 2 && expectedDay == 29;
    int expectedHour = subtractedDate.getHour();
    int expectedMin = subtractedDate.getMinute();
    int expectedSec = subtractedDate.getSecond();
    String originalDate = subtractedDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss"));

    int expectedYear = currentDate.minusDays(18250).getYear();

    String maskedDateTime = maskingProvider.mask(originalDate);

    String expected = String.format("%02d-%02d-%d %02d:%02d:%02d", expectedDay, expectedMon,
        expectedYear, expectedHour, expectedMin, expectedSec);
    String expected2 =
        expectedLeapDay
            ? String.format("%02d-%02d-%d %02d:%02d:%02d", 28, expectedMon, expectedYear,
                expectedHour, expectedMin, expectedSec)
            : null;
    assertTrue(
        maskedDateTime.equals(expected) || (expected2 != null && maskedDateTime.equals(expected2)));
  }

  @Test
  public void testDayMaxDaysAgoMask_NotEnabled() throws Exception {
    DateTimeMaskingProviderConfig maskingConfiguration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(maskingConfiguration);
    maskingConfiguration.setDayMaxDaysAgoMask(false);
    maskingConfiguration.setDayMaxDaysAgo(32850);
    maskingConfiguration.setDayShiftFromCurrentDay(18250);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(maskingConfiguration);

    String originalDate = "02-12-1914 00:00:00";
    String maskedDateTime = maskingProvider.mask(originalDate);
    assertEquals(originalDate, maskedDateTime);
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
    assertEquals(originalDate, maskedDateTime);
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
  public void testCapitalization() {
    // task date components
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setYearMask(true);
    DateTimeMaskingProvider maskingProvider = new DateTimeMaskingProvider(configuration);

    String[] names = getMonthAbbreviations();
    assertTrue(maskingProvider.mask("05-" + names[9] + "-2016").startsWith("05-" + names[9] + "-"));
    assertTrue(maskingProvider.mask("06-" + names[10].toLowerCase() + "-2016")
        .startsWith("06-" + names[10].toLowerCase() + "-"));
    assertTrue(maskingProvider.mask("07-" + names[11].toUpperCase() + "-2016")
        .startsWith("07-" + names[11].toUpperCase() + "-"));
    
    // shift constant seconds
    configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setMaskShiftDate(true);
    configuration.setMaskShiftSeconds(-1);
    maskingProvider = new DateTimeMaskingProvider(configuration);

    // shifting one second back from the start of Aug 1, gives July 31
    assertEquals("31-" + names[6] + "-2016", maskingProvider.mask("01-" + names[7] + "-2016"));
    assertEquals("31-" + names[6].toLowerCase() + "-2016",
        maskingProvider.mask("01-" + names[7].toLowerCase() + "-2016"));
    assertEquals("31-" + names[6].toUpperCase() + "-2016",
        maskingProvider.mask("01-" + names[7].toUpperCase() + "-2016"));
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
    configuration.setMinuteMask(true);
    configuration.setMinuteRangeDown(30);
    configuration.setMinuteRangeUp(12);
    maskComponent("21-05-2016 18:46:33", "21-05-2016 18:xx:33", ChronoField.MINUTE_OF_HOUR, 46, 16,
        58, true, configuration);
  }

  @Test
  public void testSecondMask() throws Exception {
    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    setAllDateTimeMaskingToFalse(configuration);
    configuration.setSecondMask(true);
    configuration.setSecondRangeDown(20);
    configuration.setSecondRangeUp(15);
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
    configuration.setYearMaxYearsAgoMask(false);
    configuration.setDayMaxDaysAgoMask(false);
    configuration.setGeneralizeMonthyearMaskAgeOver90(false);
    configuration.setGeneralizeYearMaskAgeOver90(false);
    configuration.setYearDelete(false);
    configuration.setYearDeleteNdays(false);
    configuration.setYearMask(false);
    configuration.setMonthMask(false);
    configuration.setDayMask(false);
    configuration.setHourMask(false);
    configuration.setMinuteMask(false);
    configuration.setSecondMask(false);
    configuration.setMaskShiftDate(false);
    configuration.setOverrideMask(false);
  }

  private String getYear(String originalDate) {
    return StringUtils.substring(originalDate, 6, 10);
  }

  /**
   * Gets the month abbreviations in the default locale, which can be different for each caller.
   */
  private String[] getMonthAbbreviations() {
    String[] abrvs = new String[12];
    DateTimeFormatter f = new DateTimeFormatterBuilder().appendPattern("MMM").toFormatter();
    for (int i = 0; i < 12; i++) {
      abrvs[i] = f.format(LocalDate.of(2022, i + 1, 20));
    }
    return abrvs;
  }
}
