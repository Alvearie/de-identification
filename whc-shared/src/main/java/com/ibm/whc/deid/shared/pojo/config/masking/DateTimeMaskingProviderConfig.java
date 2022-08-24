/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import java.time.format.DateTimeFormatterBuilder;
import java.util.Objects;

/*
 * Provider for masking DateTime (timestamp) objects
 */
@JsonInclude(Include.NON_NULL)
public class DateTimeMaskingProviderConfig extends MaskingProviderConfig {
  private static final long serialVersionUID = -2155188165694612384L;

  protected static final String TOO_MANY_FINAL_OPTIONS_ERROR =
      "Only one of the following options may be true: maskShiftDate, "
          + "generalizeWeekyear, generalizeMonthyear, generalizeQuarteryear, "
          + "generalizeYear, yearDelete, generalizeYearMaskAgeOver90, "
          + "generalizeMonthyearMaskAgeOver90, or one or more of these options: "
          + "yearMask, monthMask, dayMask, hourMask, minutesMask, secondsMask.";

  private int hourRangeDown = 100;
  private boolean minutesMask = true;
  private int dayRangeUpMin = 0;
  private int dayRangeUp = 0;
  private boolean hourMask = true;
  private int yearRangeDown = 10;
  private int dayRangeDownMin = 0;
  private int dayRangeDown = 7;
  private int monthRangeDown = 12;
  private boolean yearMask = true;
  private int yearRangeUp = 0;
  private boolean generalizeYear = false;
  private int secondsRangeUp = 0;
  private boolean maskShiftDate = false;
  private int secondsRangeDown = 100;
  private int minutesRangeUp = 0;
  private boolean monthMask = true;
  private boolean secondsMask = true;
  private int hourRangeUp = 0;
  private String formatFixed = null;
  private boolean generalizeWeekyear = false;
  private boolean generalizeMonthyear = false;
  private boolean dayMask = true;
  private boolean generalizeQuarteryear = false;
  private int monthRangeUp = 0;
  private int maskShiftSeconds = 0;
  private int minutesRangeDown = 100;
  private boolean yearMaxYearsAgoMask = false;
  private int yearMaxYearsAgo = 0;
  private boolean yearMaxYearsAgoOnlyYear = false;
  private int yearShiftFromCurrentYear = 0;
  private boolean dayMaxDaysAgoMask = false;
  private int dayMaxDaysAgo = 0;
  private int dayShiftFromCurrentDay = 0;
  private boolean overrideMask = false;
  private int overrideYearsPassed = 0;
  private String overrideValue = null;
  private boolean generalizeYearMaskAgeOver90 = false;
  private boolean generalizeMonthyearMaskAgeOver90 = false;
  private boolean yearDelete = false;
  private boolean yearDeleteNdays = false;
  private int yearDeleteNdaysValue = 365;

  public DateTimeMaskingProviderConfig() {
    type = MaskingProviderType.DATETIME;
  }

  public int getHourRangeDown() {
    return hourRangeDown;
  }

  public void setHourRangeDown(int hourRangeDown) {
    this.hourRangeDown = hourRangeDown;
  }

  public boolean isMinutesMask() {
    return minutesMask;
  }

  public void setMinutesMask(boolean minutesMask) {
    this.minutesMask = minutesMask;
  }

  public int getDayRangeUpMin() {
    return dayRangeUpMin;
  }

  public void setDayRangeUpMin(int dayRangeUpMin) {
    this.dayRangeUpMin = dayRangeUpMin;
  }

  public int getDayRangeUp() {
    return dayRangeUp;
  }

  public void setDayRangeUp(int dayRangeUp) {
    this.dayRangeUp = dayRangeUp;
  }

  public boolean isHourMask() {
    return hourMask;
  }

  public void setHourMask(boolean hourMask) {
    this.hourMask = hourMask;
  }

  public int getYearRangeDown() {
    return yearRangeDown;
  }

  public void setYearRangeDown(int yearRangeDown) {
    this.yearRangeDown = yearRangeDown;
  }

  public int getDayRangeDownMin() {
    return dayRangeDownMin;
  }

  public void setDayRangeDownMin(int dayRangeDownMin) {
    this.dayRangeDownMin = dayRangeDownMin;
  }

  public int getDayRangeDown() {
    return dayRangeDown;
  }

  public void setDayRangeDown(int dayRangeDown) {
    this.dayRangeDown = dayRangeDown;
  }

  public int getMonthRangeDown() {
    return monthRangeDown;
  }

  public void setMonthRangeDown(int monthRangeDown) {
    this.monthRangeDown = monthRangeDown;
  }

  public boolean isYearMask() {
    return yearMask;
  }

  public void setYearMask(boolean yearMask) {
    this.yearMask = yearMask;
  }

  public int getYearRangeUp() {
    return yearRangeUp;
  }

  public void setYearRangeUp(int yearRangeUp) {
    this.yearRangeUp = yearRangeUp;
  }

  public boolean isGeneralizeYear() {
    return generalizeYear;
  }

  public void setGeneralizeYear(boolean generalizeYear) {
    this.generalizeYear = generalizeYear;
  }

  public int getSecondsRangeUp() {
    return secondsRangeUp;
  }

  public void setSecondsRangeUp(int secondsRangeUp) {
    this.secondsRangeUp = secondsRangeUp;
  }

  public boolean isMaskShiftDate() {
    return maskShiftDate;
  }

  public void setMaskShiftDate(boolean maskShiftDate) {
    this.maskShiftDate = maskShiftDate;
  }

  public int getSecondsRangeDown() {
    return secondsRangeDown;
  }

  public void setSecondsRangeDown(int secondsRangeDown) {
    this.secondsRangeDown = secondsRangeDown;
  }

  public int getMinutesRangeUp() {
    return minutesRangeUp;
  }

  public void setMinutesRangeUp(int minutesRangeUp) {
    this.minutesRangeUp = minutesRangeUp;
  }

  public boolean isMonthMask() {
    return monthMask;
  }

  public void setMonthMask(boolean monthMask) {
    this.monthMask = monthMask;
  }

  public boolean isSecondsMask() {
    return secondsMask;
  }

  public void setSecondsMask(boolean secondsMask) {
    this.secondsMask = secondsMask;
  }

  public int getHourRangeUp() {
    return hourRangeUp;
  }

  public void setHourRangeUp(int hourRangeUp) {
    this.hourRangeUp = hourRangeUp;
  }

  public String getFormatFixed() {
    return formatFixed;
  }

  public void setFormatFixed(String formatFixed) {
    this.formatFixed = formatFixed;
  }

  public boolean isGeneralizeWeekyear() {
    return generalizeWeekyear;
  }

  public void setGeneralizeWeekyear(boolean generalizeWeekyear) {
    this.generalizeWeekyear = generalizeWeekyear;
  }

  public boolean isGeneralizeMonthyear() {
    return generalizeMonthyear;
  }

  public void setGeneralizeMonthyear(boolean generalizeMonthyear) {
    this.generalizeMonthyear = generalizeMonthyear;
  }

  public boolean isDayMask() {
    return dayMask;
  }

  public void setDayMask(boolean dayMask) {
    this.dayMask = dayMask;
  }

  public boolean isGeneralizeQuarteryear() {
    return generalizeQuarteryear;
  }

  public void setGeneralizeQuarteryear(boolean generalizeQuarteryear) {
    this.generalizeQuarteryear = generalizeQuarteryear;
  }

  public int getMonthRangeUp() {
    return monthRangeUp;
  }

  public void setMonthRangeUp(int monthRangeUp) {
    this.monthRangeUp = monthRangeUp;
  }

  public int getMaskShiftSeconds() {
    return maskShiftSeconds;
  }

  public void setMaskShiftSeconds(int maskShiftSeconds) {
    this.maskShiftSeconds = maskShiftSeconds;
  }

  public int getMinutesRangeDown() {
    return minutesRangeDown;
  }

  public void setMinutesRangeDown(int minutesRangeDown) {
    this.minutesRangeDown = minutesRangeDown;
  }

  public boolean isYearMaxYearsAgoMask() {
    return yearMaxYearsAgoMask;
  }

  public void setYearMaxYearsAgoMask(boolean yearMaxYearsAgoMask) {
    this.yearMaxYearsAgoMask = yearMaxYearsAgoMask;
  }

  public int getYearMaxYearsAgo() {
    return yearMaxYearsAgo;
  }

  public void setYearMaxYearsAgo(int yearMaxYearsAgo) {
    this.yearMaxYearsAgo = yearMaxYearsAgo;
  }

  public boolean isYearMaxYearsAgoOnlyYear() {
    return yearMaxYearsAgoOnlyYear;
  }

  public void setYearMaxYearsAgoOnlyYear(boolean yearMaxYearsAgoOnlyYear) {
    this.yearMaxYearsAgoOnlyYear = yearMaxYearsAgoOnlyYear;
  }

  public int getYearShiftFromCurrentYear() {
    return yearShiftFromCurrentYear;
  }

  public void setYearShiftFromCurrentYear(int yearShiftFromCurrentYear) {
    this.yearShiftFromCurrentYear = yearShiftFromCurrentYear;
  }

  public boolean isDayMaxDaysAgoMask() {
    return dayMaxDaysAgoMask;
  }

  public void setDayMaxDaysAgoMask(boolean dayMaxDaysAgoMask) {
    this.dayMaxDaysAgoMask = dayMaxDaysAgoMask;
  }

  public int getDayMaxDaysAgo() {
    return dayMaxDaysAgo;
  }

  public void setDayMaxDaysAgo(int dayMaxDaysAgo) {
    this.dayMaxDaysAgo = dayMaxDaysAgo;
  }

  public int getDayShiftFromCurrentDay() {
    return dayShiftFromCurrentDay;
  }

  public void setDayShiftFromCurrentDay(int dayShiftFromCurrentDay) {
    this.dayShiftFromCurrentDay = dayShiftFromCurrentDay;
  }

  public boolean isOverrideMask() {
    return overrideMask;
  }

  public void setOverrideMask(boolean overrideMask) {
    this.overrideMask = overrideMask;
  }

  public int getOverrideYearsPassed() {
    return overrideYearsPassed;
  }

  public void setOverrideYearsPassed(int overrideYearsPassed) {
    this.overrideYearsPassed = overrideYearsPassed;
  }

  public String getOverrideValue() {
    return overrideValue;
  }

  public void setOverrideValue(String overrideValue) {
    this.overrideValue = overrideValue;
  }

  public boolean isGeneralizeYearMaskAgeOver90() {
    return generalizeYearMaskAgeOver90;
  }

  public void setGeneralizeYearMaskAgeOver90(boolean generalizeYearMaskAgeOver90) {
    this.generalizeYearMaskAgeOver90 = generalizeYearMaskAgeOver90;
  }

  public boolean isGeneralizeMonthyearMaskAgeOver90() {
    return generalizeMonthyearMaskAgeOver90;
  }

  public void setGeneralizeMonthyearMaskAgeOver90(boolean generalizeMonthyearMaskAgeOver90) {
    this.generalizeMonthyearMaskAgeOver90 = generalizeMonthyearMaskAgeOver90;
  }

  public boolean isYearDelete() {
    return yearDelete;
  }

  public void setYearDelete(boolean yearDelete) {
    this.yearDelete = yearDelete;
  }

  public boolean isYearDeleteNdays() {
    return yearDeleteNdays;
  }

  public void setYearDeleteNdays(boolean yearDeleteNdays) {
    this.yearDeleteNdays = yearDeleteNdays;
  }

  public int getYearDeleteNdaysValue() {
    return yearDeleteNdaysValue;
  }

  public void setYearDeleteNdaysValue(int yearDeleteNdaysValue) {
    this.yearDeleteNdaysValue = yearDeleteNdaysValue;
  }

  @Override
  public void validate(DeidMaskingConfig maskingConfig)
      throws InvalidMaskingConfigurationException {
    super.validate(maskingConfig);
    if (formatFixed != null) {
      try {
        new DateTimeFormatterBuilder().appendPattern(formatFixed);
      } catch (IllegalArgumentException e) {
        throw new InvalidMaskingConfigurationException(
            "`formatFixed` does not contain a valid pattern: " + e.getMessage(), e);
      }
    }
    validateNotNegative(yearRangeDown, "yearRangeDown");
    validateNotNegative(yearRangeUp, "yearRangeUp");
    validateNotNegative(monthRangeDown, "monthRangeDown");
    validateNotNegative(monthRangeUp, "monthRangeUp");
    validateNotNegative(dayRangeDown, "dayRangeDown");
    validateNotNegative(dayRangeDownMin, "dayRangeDownMin");
    validateNotNegative(dayRangeUp, "dayRangeUp");
    validateNotNegative(dayRangeUpMin, "dayRangeUpMin");
    validateNotNegative(hourRangeDown, "hourRangeDown");
    validateNotNegative(hourRangeUp, "hourRangeUp");
    validateNotNegative(minutesRangeDown, "minutesRangeDown");
    validateNotNegative(minutesRangeUp, "minutesRangeUp");
    validateNotNegative(secondsRangeDown, "secondsRangeDown");
    validateNotNegative(secondsRangeUp, "secondsRangeUp");
    validateNotNegative(yearMaxYearsAgo, "yearMaxYearsAgo");
    validateNotNegative(yearShiftFromCurrentYear, "yearShiftFromCurrentYear");
    validateNotNegative(overrideYearsPassed, "overrideYearsPassed");
    validateNotNegative(dayMaxDaysAgo, "dayMaxDaysAgo");
    validateNotNegative(dayShiftFromCurrentDay, "dayShiftFromCurrentDay");
    validateNotNegative(yearDeleteNdaysValue, "yearDeleteNdaysValue");
    int activeFinalProcessing = 0;
    if (maskShiftDate) {
      activeFinalProcessing++;
    }
    if (generalizeWeekyear) {
      activeFinalProcessing++;
    }
    if (generalizeMonthyear) {
      activeFinalProcessing++;
    }
    if (generalizeQuarteryear) {
      activeFinalProcessing++;
    }
    if (generalizeYear) {
      activeFinalProcessing++;
    }
    if (yearDelete) {
      activeFinalProcessing++;
    }
    if (generalizeYearMaskAgeOver90) {
      activeFinalProcessing++;
    }
    if (generalizeMonthyearMaskAgeOver90) {
      activeFinalProcessing++;
    }
    if (yearMask || monthMask || dayMask || hourMask || minutesMask || secondsMask) {
      activeFinalProcessing++;
    }
    if (activeFinalProcessing > 1) {
      throw new InvalidMaskingConfigurationException(TOO_MANY_FINAL_OPTIONS_ERROR);
    }
  }

  private void validateNotNegative(int value, String name)
      throws InvalidMaskingConfigurationException {
    if (value < 0) {
      throw new InvalidMaskingConfigurationException(
          "`" + name + "` must be greater than or equal to 0");
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + Objects.hash(dayMask, dayMaxDaysAgo, dayMaxDaysAgoMask, dayRangeDown,
        dayRangeDownMin, dayRangeUp, dayRangeUpMin, dayShiftFromCurrentDay, formatFixed,
        generalizeMonthyear, generalizeMonthyearMaskAgeOver90, generalizeQuarteryear,
        generalizeWeekyear, generalizeYear, generalizeYearMaskAgeOver90, hourMask, hourRangeDown,
        hourRangeUp, maskShiftDate, maskShiftSeconds, minutesMask, minutesRangeDown, minutesRangeUp,
        monthMask, monthRangeDown, monthRangeUp, overrideMask, overrideValue, overrideYearsPassed,
        secondsMask, secondsRangeDown, secondsRangeUp, yearDelete, yearDeleteNdays,
        yearDeleteNdaysValue, yearMask, yearMaxYearsAgo, yearMaxYearsAgoMask,
        yearMaxYearsAgoOnlyYear, yearRangeDown, yearRangeUp, yearShiftFromCurrentYear);
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (!super.equals(obj)) {
      return false;
    }
    if (!(obj instanceof DateTimeMaskingProviderConfig)) {
      return false;
    }
    DateTimeMaskingProviderConfig other = (DateTimeMaskingProviderConfig) obj;
    return dayMask == other.dayMask && dayMaxDaysAgo == other.dayMaxDaysAgo
        && dayMaxDaysAgoMask == other.dayMaxDaysAgoMask && dayRangeDown == other.dayRangeDown
        && dayRangeDownMin == other.dayRangeDownMin && dayRangeUp == other.dayRangeUp
        && dayRangeUpMin == other.dayRangeUpMin
        && dayShiftFromCurrentDay == other.dayShiftFromCurrentDay
        && Objects.equals(formatFixed, other.formatFixed)
        && generalizeMonthyear == other.generalizeMonthyear
        && generalizeMonthyearMaskAgeOver90 == other.generalizeMonthyearMaskAgeOver90
        && generalizeQuarteryear == other.generalizeQuarteryear
        && generalizeWeekyear == other.generalizeWeekyear && generalizeYear == other.generalizeYear
        && generalizeYearMaskAgeOver90 == other.generalizeYearMaskAgeOver90
        && hourMask == other.hourMask && hourRangeDown == other.hourRangeDown
        && hourRangeUp == other.hourRangeUp && maskShiftDate == other.maskShiftDate
        && maskShiftSeconds == other.maskShiftSeconds && minutesMask == other.minutesMask
        && minutesRangeDown == other.minutesRangeDown && minutesRangeUp == other.minutesRangeUp
        && monthMask == other.monthMask && monthRangeDown == other.monthRangeDown
        && monthRangeUp == other.monthRangeUp && overrideMask == other.overrideMask
        && Objects.equals(overrideValue, other.overrideValue)
        && overrideYearsPassed == other.overrideYearsPassed && secondsMask == other.secondsMask
        && secondsRangeDown == other.secondsRangeDown && secondsRangeUp == other.secondsRangeUp
        && yearDelete == other.yearDelete && yearDeleteNdays == other.yearDeleteNdays
        && yearDeleteNdaysValue == other.yearDeleteNdaysValue && yearMask == other.yearMask
        && yearMaxYearsAgo == other.yearMaxYearsAgo
        && yearMaxYearsAgoMask == other.yearMaxYearsAgoMask
        && yearMaxYearsAgoOnlyYear == other.yearMaxYearsAgoOnlyYear
        && yearRangeDown == other.yearRangeDown && yearRangeUp == other.yearRangeUp
        && yearShiftFromCurrentYear == other.yearShiftFromCurrentYear;
  }
}
