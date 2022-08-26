/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class DateTimeMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    DateTimeMaskingProviderConfig config = new DateTimeMaskingProviderConfig();
    config.validate(null);

    config.setFormatFixed("TTT");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      System.out.println(e.getMessage());
      assertTrue(e.getMessage().contains("`formatFixed` does not contain a valid pattern: "));
      assertTrue(e.getMessage().contains("Unknown pattern letter: T"));
    }
    config.setFormatFixed("yyyy-MM-dd");
    config.validate(null);
    config.setFormatFixed(null);

    // bad pattern
    config.setYearDeleteNdaysOutputFormat("TXT");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      System.out.println(e.getMessage());
      assertTrue(e.getMessage()
          .contains("`yearDeleteNdaysOutputFormat` does not contain a valid pattern: "));
      assertTrue(e.getMessage().contains("Unknown pattern letter: T"));
    }
    // unavailable fields in pattern
    config.setYearDeleteNdaysOutputFormat("yyyy-MM-dd'T'HH:mm:ssZ");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      System.out.println(e.getMessage());
      assertTrue(e.getMessage()
          .contains("`yearDeleteNdaysOutputFormat` does not contain a valid pattern: "));
      assertTrue(e.getMessage().contains("Unsupported field: YearOfEra"));
    }
    // unavailable fields in pattern
    config.setYearDeleteNdaysOutputFormat("MM-ddZ");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      System.out.println(e.getMessage());
      assertTrue(e.getMessage()
          .contains("`yearDeleteNdaysOutputFormat` does not contain a valid pattern: "));
      assertTrue(e.getMessage().contains("Unsupported field: OffsetSeconds"));
    }
    config.setYearDeleteNdaysOutputFormat("MMM-dd");
    config.validate(null);
    config.setYearDeleteNdaysOutputFormat(null);

    config.setGeneralizeMonthYearOutputFormat("TTXT");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      System.out.println(e.getMessage());
      assertTrue(e.getMessage()
          .contains("`generalizeMonthYearOutputFormat` does not contain a valid pattern: "));
      assertTrue(e.getMessage().contains("Unknown pattern letter: T"));
    }
    // unavailable fields in pattern
    config.setGeneralizeMonthYearOutputFormat("yyyy-MM'T'HH:mm:ssZ");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      System.out.println(e.getMessage());
      assertTrue(e.getMessage()
          .contains("`generalizeMonthYearOutputFormat` does not contain a valid pattern: "));
      assertTrue(e.getMessage().contains("Unsupported field: HourOfDay"));
    }
    config.setGeneralizeMonthYearOutputFormat("yy-M");
    config.validate(null);
    config.setGeneralizeMonthYearOutputFormat(null);

    config.setGeneralizeQuarterYearOutputFormat("KXT");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      System.out.println(e.getMessage());
      assertTrue(e.getMessage()
          .contains("`generalizeQuarterYearOutputFormat` does not contain a valid pattern: "));
      assertTrue(e.getMessage().contains("Unknown pattern letter: T"));
    }
    // unavailable fields in pattern
    config.setGeneralizeQuarterYearOutputFormat("MM-yyyy:mm");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      System.out.println(e.getMessage());
      assertTrue(e.getMessage()
          .contains("`generalizeQuarterYearOutputFormat` does not contain a valid pattern: "));
      assertTrue(e.getMessage().contains("Unsupported field: MinuteOfHour"));
    }
    config.setGeneralizeQuarterYearOutputFormat("yyyy-Q'Q'");
    config.validate(null);
    config.setGeneralizeQuarterYearOutputFormat(null);

    config.setYearDeleteOutputFormat("TTD");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      System.out.println(e.getMessage());
      assertTrue(
          e.getMessage().contains("`yearDeleteOutputFormat` does not contain a valid pattern: "));
      assertTrue(e.getMessage().contains("Unknown pattern letter: T"));
    }
    config.setYearDeleteOutputFormat("MM-dd-yy");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      System.out.println(e.getMessage());
      assertTrue(
          e.getMessage().contains("`yearDeleteOutputFormat` does not contain a valid pattern: "));
      assertTrue(e.getMessage().contains("Unsupported field: YearOfEra"));
    }
    config.setYearDeleteOutputFormat("MM-dd");
    config.validate(null);
    config.setYearDeleteOutputFormat(null);

    config.setGeneralizeMonthYearMaskAgeOver90OutputFormat("XTTD");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      System.out.println(e.getMessage());
      assertTrue(e.getMessage().contains(
          "`generalizeMonthYearMaskAgeOver90OutputFormat` does not contain a valid pattern: "));
      assertTrue(e.getMessage().contains("Unknown pattern letter: T"));
    }
    config.setGeneralizeMonthYearMaskAgeOver90OutputFormat("MM-dd:yy");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      System.out.println(e.getMessage());
      assertTrue(e.getMessage().contains(
          "`generalizeMonthYearMaskAgeOver90OutputFormat` does not contain a valid pattern: "));
      assertTrue(e.getMessage().contains("Unsupported field: DayOfMonth"));
    }
    config.setGeneralizeMonthYearMaskAgeOver90OutputFormat("M/yy");
    config.validate(null);
    config.setGeneralizeMonthYearMaskAgeOver90OutputFormat(null);

    config.setHourRangeDown(-1);
    validateNegativeError(config, "hourRangeDown");
    config.setHourRangeDown(0);

    config.setDayRangeUpMin(-1);
    validateNegativeError(config, "dayRangeUpMin");
    config.setDayRangeUpMin(0);

    config.setDayRangeUp(-1);
    validateNegativeError(config, "dayRangeUp");
    config.setDayRangeUp(0);

    config.setYearRangeDown(-1);
    validateNegativeError(config, "yearRangeDown");
    config.setYearRangeDown(0);

    config.setDayRangeDownMin(-1);
    validateNegativeError(config, "dayRangeDownMin");
    config.setDayRangeDownMin(0);

    config.setDayRangeDown(-1);
    validateNegativeError(config, "dayRangeDown");
    config.setDayRangeDown(0);

    config.setMonthRangeDown(-1);
    validateNegativeError(config, "monthRangeDown");
    config.setMonthRangeDown(0);

    config.setYearRangeUp(-1);
    validateNegativeError(config, "yearRangeUp");
    config.setYearRangeUp(0);

    config.setSecondsRangeUp(-1);
    validateNegativeError(config, "secondsRangeUp");
    config.setSecondsRangeUp(0);

    config.setSecondsRangeDown(-1);
    validateNegativeError(config, "secondsRangeDown");
    config.setSecondsRangeDown(0);

    config.setMinutesRangeUp(-1);
    validateNegativeError(config, "minutesRangeUp");
    config.setMinutesRangeUp(0);

    config.setHourRangeUp(-1);
    validateNegativeError(config, "hourRangeUp");
    config.setHourRangeUp(0);

    config.setMonthRangeUp(-1);
    validateNegativeError(config, "monthRangeUp");
    config.setMonthRangeUp(0);

    config.setMinutesRangeDown(-1);
    validateNegativeError(config, "minutesRangeDown");
    config.setMinutesRangeDown(0);

    config.setYearMaxYearsAgo(-1);
    validateNegativeError(config, "yearMaxYearsAgo");
    config.setYearMaxYearsAgo(0);

    config.setYearShiftFromCurrentYear(-1);
    validateNegativeError(config, "yearShiftFromCurrentYear");
    config.setYearShiftFromCurrentYear(0);

    config.setOverrideYearsPassed(-1);
    validateNegativeError(config, "overrideYearsPassed");
    config.setOverrideYearsPassed(0);

    config.setDayMaxDaysAgo(-1);
    validateNegativeError(config, "dayMaxDaysAgo");
    config.setDayMaxDaysAgo(0);

    config.setDayShiftFromCurrentDay(-1);
    validateNegativeError(config, "dayShiftFromCurrentDay");
    config.setDayShiftFromCurrentDay(0);

    config.setYearDeleteNdaysValue(-1);
    validateNegativeError(config, "yearDeleteNdaysValue");
    config.setYearDeleteNdaysValue(0);

    config.setMaskShiftDate(true);
    config.setGeneralizeWeekyear(true);
    validateTooManyActive(config);

    config.setGeneralizeWeekyear(false);
    config.setGeneralizeMonthyear(true);
    validateTooManyActive(config);

    config.setGeneralizeMonthyear(false);
    config.setGeneralizeQuarteryear(true);
    validateTooManyActive(config);

    config.setGeneralizeQuarteryear(false);
    config.setGeneralizeYear(true);
    validateTooManyActive(config);

    config.setGeneralizeYear(false);
    config.setYearDelete(true);
    validateTooManyActive(config);

    config.setYearDelete(false);
    config.setGeneralizeYearMaskAgeOver90(true);
    validateTooManyActive(config);

    config.setGeneralizeYearMaskAgeOver90(false);
    config.setGeneralizeMonthyearMaskAgeOver90(true);
    validateTooManyActive(config);

    config.setGeneralizeMonthyearMaskAgeOver90(false);
    config.setYearMask(true);
    validateTooManyActive(config);

    config.setYearMask(false);
    config.setMonthMask(true);
    validateTooManyActive(config);

    config.setMonthMask(false);
    config.setDayMask(true);
    validateTooManyActive(config);

    config.setDayMask(false);
    config.setHourMask(true);
    validateTooManyActive(config);

    config.setHourMask(false);
    config.setMinutesMask(true);
    validateTooManyActive(config);

    config.setMinutesMask(false);
    config.setSecondsMask(true);
    validateTooManyActive(config);

    config.setMaskShiftDate(false);
    config.setYearMask(true);
    config.setMonthMask(true);
    config.setDayMask(true);
    config.setHourMask(true);
    config.setMinutesMask(true);
    config.validate(null);
  }

  private void validateNegativeError(DateTimeMaskingProviderConfig config, String name) {
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`" + name + "` must be greater than or equal to 0", e.getMessage());
    }
  }

  private void validateTooManyActive(DateTimeMaskingProviderConfig config) {
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals(DateTimeMaskingProviderConfig.TOO_MANY_FINAL_OPTIONS_ERROR, e.getMessage());
    }
  }
}
