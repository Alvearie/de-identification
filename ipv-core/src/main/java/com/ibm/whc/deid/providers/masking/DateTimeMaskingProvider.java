/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAccessor;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import org.apache.commons.lang.StringUtils;
import com.ibm.whc.deid.models.OriginalMaskedValuePair;
import com.ibm.whc.deid.providers.identifiers.DateTimeIdentifier;
import com.ibm.whc.deid.schema.FieldRelationship;
import com.ibm.whc.deid.schema.RelationshipType;
import com.ibm.whc.deid.shared.pojo.config.masking.DateDependencyMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeMaskingProviderConfig;
import com.ibm.whc.deid.util.RandomGenerators;
import com.ibm.whc.deid.util.Tuple;
import com.ibm.whc.deid.utils.log.LogCodes;

/**
 * The type Date time masking provider.
 *
 */
public class DateTimeMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -645486689214017719L;

  private static final DateTimeIdentifier dateTimeIdentifier = new DateTimeIdentifier();
  private static final DateTimeFormatter defaultDateFormat = new DateTimeFormatterBuilder()
      .appendPattern("dd/MM/yyyy").parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
      .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
      .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0).toFormatter();

  private final boolean shiftDate;
  private final int shiftSeconds;

  private final boolean generalizeWeekYear;
  private final boolean generalizeMonthYear;
  private final boolean generalizeQuarterYear;
  private final boolean generalizeYear;
  private final boolean generalizeNYearInterval;
  private final int generalizeNYearIntervalValue;
  private final int generalizeNYearIntervalStart;
  private final int generalizeNYearIntervalEnd;

  private final boolean generalizeYearMaskAgeOver90;
  private final boolean generalizeMonthYearMaskAgeOver90;

  private final boolean yearMask;
  private final int yearRangeUp;
  private final int yearRangeDown;

  private final boolean monthMask;
  private final int monthRangeUp;
  private final int monthRangeDown;

  private final boolean dayMask;
  private final int dayRangeUpMin;
  private final int dayRangeUp;
  private final int dayRangeDownMin;
  private final int dayRangeDown;

  private final boolean hourMask;
  private final int hourRangeUp;
  private final int hourRangeDown;

  private final boolean minutesMask;
  private final int minutesRangeUp;
  private final int minutesRangeDown;

  private final boolean secondsMask;
  private final int secondsRangeUp;
  private final int secondsRangeDown;

  private final boolean maxYearsAgoMask;
  private final int maxYearsAgo;
  private final boolean maxYearsAgoOnlyYear;
  private final int shiftFromCurrentYear;

  private final boolean maxDaysAgoMask;
  private final int maxDaysAgo;
  private final int shiftFromCurrentDay;

  private final boolean overrideMask;
  private final int overrideYearsPassed;
  private final String overrideValue;
  private final boolean dateYearDelete;
  private final boolean dateYearDeleteNDays;
  private final int dateYearDeleteNDaysValue;
  private final boolean dateYearDeleteNInterval;
  private final String dateYearDeleteComparedValue;
  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;

  private DateTimeFormatter fixedDateFormat = null;

  public DateTimeMaskingProvider(DateTimeMaskingProviderConfig configuration) {
    String fixedDateFormatString = configuration.getFormatFixed();

    if (fixedDateFormatString != null) {
      fixedDateFormat = new DateTimeFormatterBuilder().appendPattern(fixedDateFormatString)
          .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
          .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
          .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0).toFormatter();

    } else {
      this.fixedDateFormat = null;
    }

    this.shiftDate = configuration.isMaskShiftDate();
    this.shiftSeconds = configuration.getMaskShiftSeconds();

    this.generalizeWeekYear = configuration.isGeneralizeWeekyear();
    this.generalizeMonthYear = configuration.isGeneralizeMonthyear();
    this.generalizeQuarterYear = configuration.isGeneralizeQuarteryear();
    this.generalizeYear = configuration.isGeneralizeYear();
    this.generalizeNYearInterval = configuration.isGeneralizeNyearinterval();
    this.generalizeNYearIntervalValue = configuration.getGeneralizeNyearintervalvalue();

    this.generalizeYearMaskAgeOver90 = configuration.isGeneralizeYearMaskAgeOver90();
    this.generalizeMonthYearMaskAgeOver90 = configuration.isGeneralizeMonthyearMaskAgeOver90();

    this.generalizeNYearIntervalStart = configuration.getGeneralizeNyearintervalstart();
    this.generalizeNYearIntervalEnd = configuration.getGeneralizeNyearintervalend();

    this.yearMask = configuration.isYearMask();
    this.yearRangeUp = configuration.getYearRangeUp();
    this.yearRangeDown = configuration.getYearRangeDown();

    this.monthMask = configuration.isMonthMask();
    this.monthRangeUp = configuration.getMonthRangeUp();
    this.monthRangeDown = configuration.getMonthRangeDown();

    this.dayMask = configuration.isDayMask();
    this.dayRangeUpMin = configuration.getDayRangeUpMin();
    this.dayRangeUp = configuration.getDayRangeUp();
    this.dayRangeDownMin = configuration.getDayRangeDownMin();
    this.dayRangeDown = configuration.getDayRangeDown();

    this.hourMask = configuration.isHourMask();
    this.hourRangeUp = configuration.getHourRangeUp();
    this.hourRangeDown = configuration.getHourRangeDown();

    this.minutesMask = configuration.isMinutesMask();
    this.minutesRangeUp = configuration.getMinutesRangeUp();
    this.minutesRangeDown = configuration.getMinutesRangeDown();

    this.secondsMask = configuration.isSecondsMask();
    this.secondsRangeUp = configuration.getSecondsRangeUp();
    this.secondsRangeDown = configuration.getSecondsRangeDown();

    this.maxYearsAgoMask = configuration.isYearMaxYearsAgoMask();
    this.maxYearsAgo = configuration.getYearMaxYearsAgo();
    this.maxYearsAgoOnlyYear = configuration.isYearMaxYearsAgoOnlyYear();
    this.shiftFromCurrentYear = configuration.getYearShiftFromCurrentYear();
    this.maxDaysAgoMask = configuration.isDayMaxDaysAgoMask();
    this.maxDaysAgo = configuration.getDayMaxDaysAgo();
    this.shiftFromCurrentDay = configuration.getDayShiftFromCurrentDay();

    this.overrideMask = configuration.isOverrideMask();
    this.overrideYearsPassed = configuration.getOverrideYearsPassed();
    this.overrideValue = configuration.getOverrideValue();

    this.dateYearDelete = configuration.isYearDelete();
    this.dateYearDeleteNDays = configuration.isYearDeleteNdays();
    this.dateYearDeleteNDaysValue = configuration.getYearDeleteNdaysValue();
    this.dateYearDeleteNInterval = configuration.isYearDeleteNinterval();
    this.dateYearDeleteComparedValue = configuration.getYearDeleteNointervalComparedateValue();

    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  public DateTimeMaskingProvider(DateDependencyMaskingProviderConfig dateDependencyConfig) {

    // Set some default values needed for date dependency
    this.dateYearDelete = false;
    this.dateYearDeleteNDays = false;
    this.dateYearDeleteNInterval = true;
    this.yearMask = false;
    this.monthMask = false;
    this.dayMask = false;
    this.hourMask = false;
    this.minutesMask = false;
    this.secondsMask = false;

    this.dateYearDeleteNDaysValue = dateDependencyConfig.getDateYearDeleteNDaysValue();
    this.dateYearDeleteComparedValue =
        dateDependencyConfig.getDatetimeyearDeleteNIntervalCompareDateValue();

    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();
    String fixedDateFormatString = configuration.getFormatFixed();

    if (fixedDateFormatString != null) {
      fixedDateFormat = new DateTimeFormatterBuilder().appendPattern(fixedDateFormatString)
          .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
          .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
          .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0).toFormatter();

    } else {
      this.fixedDateFormat = null;
    }

    this.shiftDate = configuration.isMaskShiftDate();
    this.shiftSeconds = configuration.getMaskShiftSeconds();

    this.generalizeWeekYear = configuration.isGeneralizeWeekyear();
    this.generalizeMonthYear = configuration.isGeneralizeMonthyear();
    this.generalizeQuarterYear = configuration.isGeneralizeQuarteryear();
    this.generalizeYear = configuration.isGeneralizeYear();
    this.generalizeNYearInterval = configuration.isGeneralizeNyearinterval();
    this.generalizeNYearIntervalValue = configuration.getGeneralizeNyearintervalvalue();

    this.generalizeYearMaskAgeOver90 = configuration.isGeneralizeYearMaskAgeOver90();
    this.generalizeMonthYearMaskAgeOver90 = configuration.isGeneralizeMonthyearMaskAgeOver90();

    this.generalizeNYearIntervalStart = configuration.getGeneralizeNyearintervalstart();
    this.generalizeNYearIntervalEnd = configuration.getGeneralizeNyearintervalend();

    this.yearRangeUp = configuration.getYearRangeUp();
    this.yearRangeDown = configuration.getYearRangeDown();

    this.monthRangeUp = configuration.getMonthRangeUp();
    this.monthRangeDown = configuration.getMonthRangeDown();

    this.dayRangeUpMin = configuration.getDayRangeUpMin();
    this.dayRangeUp = configuration.getDayRangeUp();
    this.dayRangeDownMin = configuration.getDayRangeDownMin();
    this.dayRangeDown = configuration.getDayRangeDown();

    this.hourRangeUp = configuration.getHourRangeUp();
    this.hourRangeDown = configuration.getHourRangeDown();

    this.minutesRangeUp = configuration.getMinutesRangeUp();
    this.minutesRangeDown = configuration.getMinutesRangeDown();

    this.secondsRangeUp = configuration.getSecondsRangeUp();
    this.secondsRangeDown = configuration.getSecondsRangeDown();

    this.maxYearsAgoMask = configuration.isYearMaxYearsAgoMask();
    this.maxYearsAgo = configuration.getYearMaxYearsAgo();
    this.maxYearsAgoOnlyYear = configuration.isYearMaxYearsAgoOnlyYear();
    this.shiftFromCurrentYear = configuration.getYearShiftFromCurrentYear();
    this.maxDaysAgoMask = configuration.isDayMaxDaysAgoMask();
    this.maxDaysAgo = configuration.getDayMaxDaysAgo();
    this.shiftFromCurrentDay = configuration.getDayShiftFromCurrentDay();

    this.overrideMask = configuration.isOverrideMask();
    this.overrideYearsPassed = configuration.getOverrideYearsPassed();
    this.overrideValue = configuration.getOverrideValue();

    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }


  private Date getDate(DateTimeFormatter f, String identifier) {
    TemporalAccessor d = null;
    try {
      d = f.parse(identifier);
    } catch (DateTimeParseException e) {
      if (log.isWarnEnabled()) {
        log.logWarn(LogCodes.WPH1012W, "DateTimeParseException was thrown when parsing the date");
      }
      return null;
    }

    return Date.from(Instant.from(d));
  }

  /*
   * We can use LocalDate (org.joda.time.LocalDate) instead: LocalDate birthDate = new
   * LocalDate(birthYear, birthMonth, birthDay); LocalDate todayDate = new LocalDate(); Years ageYrs
   * = Years.yearsBetween(birthDate, todayDate) But would need to convert the birth date back into a
   * Calendar object if that's required. So will use the Calendar implementation instead
   */
  public LocalDateTime maskAgeHelper(Calendar cal) {

    LocalDateTime currentDate = LocalDateTime.now();
    LocalDateTime givenDate = LocalDateTime.ofInstant(cal.toInstant(), ZoneId.systemDefault());

    // If age is 90 or over
    if (ChronoUnit.YEARS.between(givenDate, currentDate) >= 89
        && ChronoUnit.YEARS.between(givenDate, currentDate.minusDays(1)) >= 89) {
      // Set the age to exactly 90 from today's date
      givenDate = currentDate.minusYears(90);
    }

    return givenDate;
  }

  @Override
  public String mask(String identifier, String fieldName, FieldRelationship fieldRelationship,
      Map<String, OriginalMaskedValuePair> maskedValues) {
    try {
      if (identifier == null) {
        debugFaultyInput("identifier");
        return null;
      }

      String operand = fieldRelationship.getOperands()[0].getName();
      String baseMaskedValue = maskedValues.get(operand).getMasked();

      RelationshipType relationshipType = fieldRelationship.getRelationshipType();
      if (relationshipType == RelationshipType.EQUALS) {
        return baseMaskedValue;
      }

      DateTimeFormatter f = dateTimeIdentifier.matchingFormat(baseMaskedValue);
      if (f == null) {
        return RandomGenerators.generateRandomDate(defaultDateFormat.withZone(ZoneOffset.UTC));
      }

      Date d = getDate(f, baseMaskedValue);
      if (d == null) {
        return mask(identifier);
      }

      Date originalDate = getDate(f, identifier);
      Date operandOriginalDate = getDate(f, maskedValues.get(operand).getOriginal());

      if (operandOriginalDate == null) {
        if (log.isDebugEnabled()) {
          log.logDebug(LogCodes.WPH1000I, "operandOriginalDate is null");
        }
        return mask(identifier);
      }

      if (originalDate == null) {
        if (log.isDebugEnabled()) {
          log.logDebug(LogCodes.WPH1000I, "originalDate is null");
        }
        return mask(identifier);
      }

      long diff = (operandOriginalDate.getTime() - originalDate.getTime());

      Calendar cal = Calendar.getInstance();
      cal.setTime(d);

      switch (relationshipType) {
        case LESS:
          cal.setTimeInMillis(cal.getTimeInMillis() - diff);
          break;
        case GREATER:
          cal.setTimeInMillis(cal.getTimeInMillis() + diff);
          break;
        default:
          // XXX we should never reach this point!
          return mask(identifier);
      }

      return f.format(cal.getTime().toInstant());
    } catch (Exception e) {
      logException(e);
      return null;
    }
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    boolean isLowercase = identifier.equals(identifier.toLowerCase());
    boolean isUppercase = identifier.equals(identifier.toUpperCase());

    DateTimeFormatter f = null;
    Date d = null;

    if (fixedDateFormat == null) {
      Tuple<DateTimeFormatter, Date> tuple = dateTimeIdentifier.parse(identifier);
      if (tuple == null) {
        warnFaultyInput("tuple");
        if (unspecifiedValueHandling == 2) {
          return RandomGenerators.generateRandomDate(defaultDateFormat.withZone(ZoneOffset.UTC));
        } else if (unspecifiedValueHandling == 3) {
          return unspecifiedValueReturnMessage;
        } else {
          return null;
        }
      }

      f = tuple.getFirst();
      d = tuple.getSecond();
    } else {
      try {
        f = fixedDateFormat;
        TemporalAccessor t = f.withZone(ZoneId.systemDefault()).parse(identifier);
        d = Date.from(Instant.from(t));
      } catch (DateTimeParseException e) {
        return RandomGenerators.generateRandomDate(f.withZone(ZoneOffset.UTC));
      }
    }

    Calendar cal = Calendar.getInstance();
    cal.setTime(d);
    Calendar originalCal = (Calendar) cal.clone();

    if (overrideMask) {
      LocalDateTime currentDate = LocalDateTime.now();
      LocalDateTime givenDate = LocalDateTime.ofInstant(cal.toInstant(), ZoneId.systemDefault());
      if (ChronoUnit.YEARS.between(givenDate, currentDate) >= overrideYearsPassed) {
        if (StringUtils.isNotEmpty(overrideValue))
          return overrideValue;
        else
          return String.valueOf(overrideYearsPassed) + "+";
      }
    }

    if (shiftDate) {
      cal.add(Calendar.SECOND, this.shiftSeconds);
      return f.withZone(ZoneId.systemDefault()).format(cal.getTime().toInstant());
    }

    if (generalizeWeekYear) {
      int originalYear = cal.get(Calendar.YEAR);
      int originalWeek = cal.get(Calendar.WEEK_OF_YEAR);
      return String.format("%02d/%d", originalWeek, originalYear);
    }

    if (generalizeMonthYear) {
      int originalYear = cal.get(Calendar.YEAR);
      int originalMonth = cal.get(Calendar.MONTH);
      return String.format("%02d/%d", originalMonth + 1, originalYear);
    }

    if (generalizeQuarterYear) {
      int originalYear = cal.get(Calendar.YEAR);
      int originalMonth = cal.get(Calendar.MONTH);

      int quarter = originalMonth / 3;
      return String.format("%02d/%d", quarter + 1, originalYear);
    }

    if (generalizeYear) {
      int originalYear = cal.get(Calendar.YEAR);
      return (new StringBuffer(originalYear + "")).toString();
    }

    if (generalizeNYearInterval) {
      int originalYear = cal.get(Calendar.YEAR);
      // If original year is before start year, or invalid interval,
      // then return empty string
      if (originalYear < generalizeNYearIntervalStart || generalizeNYearIntervalValue < 1) {
        return "";
      }

      int yearsAfterStart = originalYear - generalizeNYearIntervalStart;
      int start = originalYear - (yearsAfterStart % generalizeNYearIntervalValue);
      int end = start + generalizeNYearIntervalValue - 1;

      // If end falls after end year, then return empty string
      if (generalizeNYearIntervalEnd != 0 && end > generalizeNYearIntervalEnd) {
        return "";
      }

      return String.format("%d-%d", start, end);
    }

    if (generalizeYearMaskAgeOver90) {
      // Mask birth date if age over 89
      LocalDateTime modifiedDate = maskAgeHelper(cal);

      // Maintain year
      return String.format("%d", modifiedDate.getYear());
    }

    if (generalizeMonthYearMaskAgeOver90) {
      // Mask birth date if age over 89
      LocalDateTime modifiedDate = maskAgeHelper(cal);

      // Maintain year & month
      return String.format("%02d/%d", modifiedDate.getMonthValue(), modifiedDate.getYear());
    }

    if (yearMask) {
      int originalYear = cal.get(Calendar.YEAR);
      int randomYear = RandomGenerators.randomWithinRange(originalYear, yearRangeDown, yearRangeUp);
      cal.add(Calendar.YEAR, randomYear - originalYear);
    }

    if (monthMask) {
      int originalMonth = cal.get(Calendar.MONTH);
      int randomMonth =
          RandomGenerators.randomWithinRange(originalMonth, monthRangeDown, monthRangeUp);
      cal.add(Calendar.MONTH, randomMonth - originalMonth);
    }

    if (dayMask) {
      int originalDay = cal.get(Calendar.DAY_OF_MONTH);
      int randomDay = RandomGenerators.randomWithinRange(originalDay, dayRangeDownMin, dayRangeDown,
          dayRangeUpMin, dayRangeUp);
      cal.add(Calendar.DAY_OF_MONTH, randomDay - originalDay);
    }

    if (hourMask) {
      int originalHour = cal.get(Calendar.HOUR_OF_DAY);
      int randomHour = RandomGenerators.randomWithinRange(originalHour, hourRangeDown, hourRangeUp);
      cal.add(Calendar.HOUR_OF_DAY, randomHour - originalHour);
    }

    if (minutesMask) {
      int originalMinutes = cal.get(Calendar.MINUTE);
      int randomMinutes =
          RandomGenerators.randomWithinRange(originalMinutes, minutesRangeDown, minutesRangeUp);
      cal.add(Calendar.MINUTE, randomMinutes - originalMinutes);
    }

    if (secondsMask) {
      int originalSeconds = cal.get(Calendar.SECOND);
      int randomSeconds =
          RandomGenerators.randomWithinRange(originalSeconds, secondsRangeDown, secondsRangeUp);
      cal.add(Calendar.SECOND, randomSeconds - originalSeconds);
    }

    // To preserve date fields if option set to false
    if (!yearMask) {
      cal.set(Calendar.YEAR, originalCal.get(Calendar.YEAR));
    }

    if (!monthMask) {
      cal.set(Calendar.MONTH, originalCal.get(Calendar.MONTH));
    }

    if (!dayMask) {
      cal.set(Calendar.DAY_OF_MONTH, originalCal.get(Calendar.DAY_OF_MONTH));
    }

    if (!hourMask) {
      cal.set(Calendar.HOUR_OF_DAY, originalCal.get(Calendar.HOUR_OF_DAY));
    }

    if (!minutesMask) {
      cal.set(Calendar.MINUTE, originalCal.get(Calendar.MINUTE));
    }

    if (!secondsMask) {
      cal.set(Calendar.SECOND, originalCal.get(Calendar.SECOND));
    }

    if (maxYearsAgoMask) {
      LocalDateTime currentDate = LocalDateTime.now();
      LocalDateTime givenDate = LocalDateTime.ofInstant(cal.toInstant(), ZoneId.systemDefault());
      LocalDateTime currDateAfterMinus;
      if (currentDate.getYear() % 4 == 0 && currentDate.getMonthValue() == 3
          && currentDate.getDayOfMonth() == 1) {
        currDateAfterMinus = currentDate.minusDays(2);
      } else {
        currDateAfterMinus = currentDate.minusDays(1);
      }
      if (ChronoUnit.YEARS.between(givenDate, currentDate) >= maxYearsAgo
          && ChronoUnit.YEARS.between(givenDate, currDateAfterMinus) >= maxYearsAgo) {
        LocalDateTime adjustedCurrentDate = currentDate.minusYears(shiftFromCurrentYear);
        cal.set(Calendar.YEAR, adjustedCurrentDate.getYear());
        if (maxYearsAgoOnlyYear) {
          return String.format("%d", cal.get(Calendar.YEAR));
        }
      }
    }

    if (maxDaysAgoMask) {
      LocalDateTime currentDate = LocalDateTime.now();
      LocalDateTime givenDate = LocalDateTime.ofInstant(cal.toInstant(), ZoneId.systemDefault());
      if (ChronoUnit.DAYS.between(givenDate, currentDate) > maxDaysAgo) {
        LocalDateTime adjustedCurrentDate = currentDate.minusDays(shiftFromCurrentDay);
        cal.set(Calendar.YEAR, adjustedCurrentDate.getYear());
        if (maxYearsAgoOnlyYear) {
          return String.format("%d", cal.get(Calendar.YEAR));
        }
      }
    }

    if (dateYearDelete) {
      int originalMonth = cal.get(Calendar.MONTH);
      int originalDay = cal.get(Calendar.DAY_OF_MONTH);
      return String.format("%02d/%02d", originalDay, originalMonth + 1);
    }

    if (dateYearDeleteNDays) {
      LocalDateTime givenDate = LocalDateTime.ofInstant(cal.toInstant(), ZoneId.systemDefault());
      LocalDateTime subtractNDaysDate = LocalDateTime.now().minusDays(dateYearDeleteNDaysValue);

      if (givenDate.isAfter(subtractNDaysDate)) {
        int originalMonth = cal.get(Calendar.MONTH);
        int originalDay = cal.get(Calendar.DAY_OF_MONTH);
        return String.format("%02d/%02d", originalDay, originalMonth + 1);
      }
    }

    if (dateYearDeleteNInterval) {
      if (dateYearDeleteComparedValue != null) {
        LocalDateTime givenDate = LocalDateTime.ofInstant(cal.toInstant(), ZoneId.systemDefault());
        LocalDateTime comparedDate = LocalDateTime
            .ofInstant(Instant.from(f.parse(dateYearDeleteComparedValue)), ZoneId.systemDefault());

        if (Math
            .abs(ChronoUnit.DAYS.between(givenDate, comparedDate)) <= dateYearDeleteNDaysValue) {
          int originalMonth = cal.get(Calendar.MONTH);
          int originalDay = cal.get(Calendar.DAY_OF_MONTH);
          return String.format("%02d/%02d", originalDay, originalMonth + 1);
        }
      }
    }

    String result = null;

    if (f.toString().equals(DateTimeFormatter.ISO_OFFSET_DATE_TIME.toString())) {
      result = f.withZone(ZoneOffset.UTC).format(cal.getTime().toInstant());
    } else {
      result = f.withZone(ZoneId.systemDefault()).format(cal.getTime().toInstant());
    }

    // solving the issues with cases
    if (isLowercase) {
      return result.toLowerCase();
    } else if (isUppercase) {
      return result.toUpperCase();
    } else {
      return result;
    }
  }
}
