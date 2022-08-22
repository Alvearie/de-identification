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
      assertTrue(e.getMessage().contains("`formatFixed` is not valid: "));
    }

    config.setFormatFixed("yyyy-MM-dd");
    config.validate(null);

    config.setFormatFixed(null);

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
  }

  private void validateNegativeError(DateTimeMaskingProviderConfig config, String name) {
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`" + name + "` must be greater than or equal to 0", e.getMessage());
    }
  }
}
