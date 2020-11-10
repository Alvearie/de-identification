/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;

/*
 * Provider for masking DateTime (timestamp) objects
 */
@JsonInclude(Include.NON_NULL)
public class DateTimeMaskingProviderConfig extends MaskingProviderConfig {
  private static final long serialVersionUID = -2155188165694612384L;

  int hourRangeDown = 100;
  boolean minutesMask = true;
  int dayRangeUpMin = 0;
  int dayRangeUp = 0;
  boolean hourMask = true;
  int yearRangeDown = 10;
  int dayRangeDownMin = 0;
  int dayRangeDown = 7;
  int monthRangeDown = 12;
  boolean yearMask = true;
  int yearRangeUp = 0;
  boolean generalizeYear = false;
  int secondsRangeUp = 0;
  boolean maskShiftDate = false;
  int secondsRangeDown = 100;
  int minutesRangeUp = 0;
  boolean monthMask = true;
  boolean generalizeNyearinterval = false;
  boolean secondsMask = true;
  int hourRangeUp = 0;
  String formatFixed = null;
  boolean generalizeWeekyear = false;
  boolean generalizeMonthyear = false;
  boolean dayMask = true;
  boolean generalizeQuarteryear = false;
  int generalizeNyearintervalvalue = 0;
  int generalizeNyearintervalstart = 0;
  int generalizeNyearintervalend = 0;
  boolean yearDeleteNinterval = false;
  String yearDeleteNointervalComparedateValue = null;
  int monthRangeUp = 0;
  int maskShiftSeconds = 0;
  int minutesRangeDown = 100;
  boolean yearMaxYearsAgoMask = false;
  int yearMaxYearsAgo = 0;
  boolean yearMaxYearsAgoOnlyYear = false;
  int yearShiftFromCurrentYear = 0;
  boolean dayMaxDaysAgoMask = false;
  int dayMaxDaysAgo = 0;
  int dayShiftFromCurrentDay = 0;
  boolean overrideMask = false;
  int overrideYearsPassed = 0;
  String overrideValue = null;
  boolean generalizeYearMaskAgeOver90 = false;
  boolean generalizeMonthyearMaskAgeOver90 = false;

  boolean yearDelete = false;
  boolean yearDeleteNdays = false;
  int yearDeleteNdaysValue = 365;

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

  public boolean isGeneralizeNyearinterval() {
    return generalizeNyearinterval;
  }

  public void setGeneralizeNyearinterval(boolean generalizeNyearinterval) {
    this.generalizeNyearinterval = generalizeNyearinterval;
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

  public int getGeneralizeNyearintervalvalue() {
    return generalizeNyearintervalvalue;
  }

  public void setGeneralizeNyearintervalvalue(int generalizeNyearintervalvalue) {
    this.generalizeNyearintervalvalue = generalizeNyearintervalvalue;
  }

  public int getGeneralizeNyearintervalstart() {
    return generalizeNyearintervalstart;
  }

  public void setGeneralizeNyearintervalstart(int generalizeNyearintervalstart) {
    this.generalizeNyearintervalstart = generalizeNyearintervalstart;
  }

  public int getGeneralizeNyearintervalend() {
    return generalizeNyearintervalend;
  }

  public void setGeneralizeNyearintervalend(int generalizeNyearintervalend) {
    this.generalizeNyearintervalend = generalizeNyearintervalend;
  }

  public boolean isYearDeleteNinterval() {
    return yearDeleteNinterval;
  }

  public void setYearDeleteNinterval(boolean yearDeleteNinterval) {
    this.yearDeleteNinterval = yearDeleteNinterval;
  }

  public String getYearDeleteNointervalComparedateValue() {
    return yearDeleteNointervalComparedateValue;
  }

  public void setYearDeleteNointervalComparedateValue(String yearDeleteNointervalComparedateValue) {
    this.yearDeleteNointervalComparedateValue = yearDeleteNointervalComparedateValue;
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
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (dayMask ? 1231 : 1237);
    result = prime * result + dayMaxDaysAgo;
    result = prime * result + (dayMaxDaysAgoMask ? 1231 : 1237);
    result = prime * result + dayRangeDown;
    result = prime * result + dayRangeDownMin;
    result = prime * result + dayRangeUp;
    result = prime * result + dayRangeUpMin;
    result = prime * result + dayShiftFromCurrentDay;
    result = prime * result + ((formatFixed == null) ? 0 : formatFixed.hashCode());
    result = prime * result + (generalizeMonthyear ? 1231 : 1237);
    result = prime * result + (generalizeMonthyearMaskAgeOver90 ? 1231 : 1237);
    result = prime * result + (generalizeNyearinterval ? 1231 : 1237);
    result = prime * result + generalizeNyearintervalend;
    result = prime * result + generalizeNyearintervalstart;
    result = prime * result + generalizeNyearintervalvalue;
    result = prime * result + (generalizeQuarteryear ? 1231 : 1237);
    result = prime * result + (generalizeWeekyear ? 1231 : 1237);
    result = prime * result + (generalizeYear ? 1231 : 1237);
    result = prime * result + (generalizeYearMaskAgeOver90 ? 1231 : 1237);
    result = prime * result + (hourMask ? 1231 : 1237);
    result = prime * result + hourRangeDown;
    result = prime * result + hourRangeUp;
    result = prime * result + (maskShiftDate ? 1231 : 1237);
    result = prime * result + maskShiftSeconds;
    result = prime * result + (minutesMask ? 1231 : 1237);
    result = prime * result + minutesRangeDown;
    result = prime * result + minutesRangeUp;
    result = prime * result + (monthMask ? 1231 : 1237);
    result = prime * result + monthRangeDown;
    result = prime * result + monthRangeUp;
    result = prime * result + (overrideMask ? 1231 : 1237);
    result = prime * result + ((overrideValue == null) ? 0 : overrideValue.hashCode());
    result = prime * result + overrideYearsPassed;
    result = prime * result + (secondsMask ? 1231 : 1237);
    result = prime * result + secondsRangeDown;
    result = prime * result + secondsRangeUp;
    result = prime * result + (yearDelete ? 1231 : 1237);
    result = prime * result + (yearDeleteNdays ? 1231 : 1237);
    result = prime * result + yearDeleteNdaysValue;
    result = prime * result + (yearDeleteNinterval ? 1231 : 1237);
    result = prime * result + ((yearDeleteNointervalComparedateValue == null) ? 0
        : yearDeleteNointervalComparedateValue.hashCode());
    result = prime * result + (yearMask ? 1231 : 1237);
    result = prime * result + yearMaxYearsAgo;
    result = prime * result + (yearMaxYearsAgoMask ? 1231 : 1237);
    result = prime * result + (yearMaxYearsAgoOnlyYear ? 1231 : 1237);
    result = prime * result + yearRangeDown;
    result = prime * result + yearRangeUp;
    result = prime * result + yearShiftFromCurrentYear;
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (!super.equals(obj))
      return false;
    if (getClass() != obj.getClass())
      return false;
    DateTimeMaskingProviderConfig other = (DateTimeMaskingProviderConfig) obj;
    if (dayMask != other.dayMask)
      return false;
    if (dayMaxDaysAgo != other.dayMaxDaysAgo)
      return false;
    if (dayMaxDaysAgoMask != other.dayMaxDaysAgoMask)
      return false;
    if (dayRangeDown != other.dayRangeDown)
      return false;
    if (dayRangeDownMin != other.dayRangeDownMin)
      return false;
    if (dayRangeUp != other.dayRangeUp)
      return false;
    if (dayRangeUpMin != other.dayRangeUpMin)
      return false;
    if (dayShiftFromCurrentDay != other.dayShiftFromCurrentDay)
      return false;
    if (formatFixed == null) {
      if (other.formatFixed != null)
        return false;
    } else if (!formatFixed.equals(other.formatFixed))
      return false;
    if (generalizeMonthyear != other.generalizeMonthyear)
      return false;
    if (generalizeMonthyearMaskAgeOver90 != other.generalizeMonthyearMaskAgeOver90)
      return false;
    if (generalizeNyearinterval != other.generalizeNyearinterval)
      return false;
    if (generalizeNyearintervalend != other.generalizeNyearintervalend)
      return false;
    if (generalizeNyearintervalstart != other.generalizeNyearintervalstart)
      return false;
    if (generalizeNyearintervalvalue != other.generalizeNyearintervalvalue)
      return false;
    if (generalizeQuarteryear != other.generalizeQuarteryear)
      return false;
    if (generalizeWeekyear != other.generalizeWeekyear)
      return false;
    if (generalizeYear != other.generalizeYear)
      return false;
    if (generalizeYearMaskAgeOver90 != other.generalizeYearMaskAgeOver90)
      return false;
    if (hourMask != other.hourMask)
      return false;
    if (hourRangeDown != other.hourRangeDown)
      return false;
    if (hourRangeUp != other.hourRangeUp)
      return false;
    if (maskShiftDate != other.maskShiftDate)
      return false;
    if (maskShiftSeconds != other.maskShiftSeconds)
      return false;
    if (minutesMask != other.minutesMask)
      return false;
    if (minutesRangeDown != other.minutesRangeDown)
      return false;
    if (minutesRangeUp != other.minutesRangeUp)
      return false;
    if (monthMask != other.monthMask)
      return false;
    if (monthRangeDown != other.monthRangeDown)
      return false;
    if (monthRangeUp != other.monthRangeUp)
      return false;
    if (overrideMask != other.overrideMask)
      return false;
    if (overrideValue == null) {
      if (other.overrideValue != null)
        return false;
    } else if (!overrideValue.equals(other.overrideValue))
      return false;
    if (overrideYearsPassed != other.overrideYearsPassed)
      return false;
    if (secondsMask != other.secondsMask)
      return false;
    if (secondsRangeDown != other.secondsRangeDown)
      return false;
    if (secondsRangeUp != other.secondsRangeUp)
      return false;
    if (yearDelete != other.yearDelete)
      return false;
    if (yearDeleteNdays != other.yearDeleteNdays)
      return false;
    if (yearDeleteNdaysValue != other.yearDeleteNdaysValue)
      return false;
    if (yearDeleteNinterval != other.yearDeleteNinterval)
      return false;
    if (yearDeleteNointervalComparedateValue == null) {
      if (other.yearDeleteNointervalComparedateValue != null)
        return false;
    } else if (!yearDeleteNointervalComparedateValue
        .equals(other.yearDeleteNointervalComparedateValue))
      return false;
    if (yearMask != other.yearMask)
      return false;
    if (yearMaxYearsAgo != other.yearMaxYearsAgo)
      return false;
    if (yearMaxYearsAgoMask != other.yearMaxYearsAgoMask)
      return false;
    if (yearMaxYearsAgoOnlyYear != other.yearMaxYearsAgoOnlyYear)
      return false;
    if (yearRangeDown != other.yearRangeDown)
      return false;
    if (yearRangeUp != other.yearRangeUp)
      return false;
    if (yearShiftFromCurrentYear != other.yearShiftFromCurrentYear)
      return false;
    return true;
  }
}
