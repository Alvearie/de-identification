/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.temporal.Temporal;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalQueries;
import java.util.Calendar;
import java.util.Date;
import org.apache.commons.lang.StringUtils;
import com.ibm.whc.deid.providers.identifiers.DateTimeIdentifier;
import com.ibm.whc.deid.providers.identifiers.DateTimeIdentifier.DateTimeParseResult;
import com.ibm.whc.deid.shared.pojo.config.masking.DateDependencyMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeMaskingProviderConfig;
import com.ibm.whc.deid.util.RandomGenerators;
import com.ibm.whc.deid.util.Tuple;

/**
 * The type Date time masking provider.
 *
 */
public class DateTimeMaskingProvider extends AbstractMaskingProvider {

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
  private final boolean yearDelete;
  private final boolean yearDeleteNDays;
  private final int yearDeleteNDaysValue;
  private final boolean yearDeleteNInterval;
  private final String yearDeleteNointervalComparedateValue;
  private final String fixedFormatString;

  public DateTimeMaskingProvider(DateTimeMaskingProviderConfig configuration) {
    super(configuration);

    this.fixedFormatString = configuration.getFormatFixed();

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

    this.yearDelete = configuration.isYearDelete();
    this.yearDeleteNDays = configuration.isYearDeleteNdays();
    this.yearDeleteNDaysValue = configuration.getYearDeleteNdaysValue();
    this.yearDeleteNInterval = configuration.isYearDeleteNinterval();
    this.yearDeleteNointervalComparedateValue = configuration.getYearDeleteNointervalComparedateValue();
  }

  public DateTimeMaskingProvider(DateDependencyMaskingProviderConfig dateDependencyConfig,
      String compareDateValue) {
    super(dateDependencyConfig);

    // Set some default values needed for date dependency
    this.yearDelete = false;
    this.yearDeleteNDays = false;
    this.yearDeleteNInterval = true;
    this.yearMask = false;
    this.monthMask = false;
    this.dayMask = false;
    this.hourMask = false;
    this.minutesMask = false;
    this.secondsMask = false;

    this.yearDeleteNDaysValue = dateDependencyConfig.getDateYearDeleteNDaysValue();
    this.yearDeleteNointervalComparedateValue = compareDateValue;

    DateTimeMaskingProviderConfig configuration = new DateTimeMaskingProviderConfig();

    this.fixedFormatString = configuration.getFormatFixed();

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
    TemporalAccessor d = null;

    if (this.fixedFormatString != null && !this.fixedFormatString.trim().isEmpty()) {
      try {
        final DateTimeFormatter fixedFormatter =
            new DateTimeFormatterBuilder().parseCaseInsensitive()
                .appendPattern(this.fixedFormatString)
                .parseDefaulting(ChronoField.MONTH_OF_YEAR, 1)
                .parseDefaulting(ChronoField.DAY_OF_MONTH, 1)
                .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0).toFormatter();
        try {
          d = fixedFormatter.parse(identifier);
          f = fixedFormatter;
        } catch (DateTimeParseException e) {
          return applyUnexpectedValueHandling(identifier,
              () -> RandomGenerators.generateRandomDate(fixedFormatter.withZone(ZoneOffset.UTC)));
        }
      } catch (IllegalArgumentException e) {
        // thrown if the pattern is not a valid datetime pattern
        return applyUnexpectedValueHandling(identifier,
            () -> RandomGenerators.generateRandomDate(defaultDateFormat.withZone(ZoneOffset.UTC)));
      }

    } else {
      DateTimeParseResult parseResult = dateTimeIdentifier.parse(identifier);
      if (parseResult == null) {
        return applyUnexpectedValueHandling(identifier,
            () -> RandomGenerators.generateRandomDate(defaultDateFormat.withZone(ZoneOffset.UTC)));
      }
      f = parseResult.getFormatter();
      d = parseResult.getValue();
    }

    // keep as much time zone information as available from the original    
    Temporal datetime;
    boolean datetimeHasOffset;
    if (d.query(TemporalQueries.zoneId()) != null) {
      datetime = d.query(ZonedDateTime::from);
      datetimeHasOffset = true;
    } else if (d.query(TemporalQueries.offset()) != null) {
      datetime = d.query(OffsetDateTime::from);
      datetimeHasOffset = true;
    } else {
      datetime = d.query(LocalDateTime::from);
      datetimeHasOffset = false;
    }

    // Return a given, constant value if the input date is at least a given number of years ago.
    // Otherwise, continue processing.
    if (overrideMask) {
      Temporal now = datetimeHasOffset ? ZonedDateTime.now() : LocalDateTime.now();
      if (ChronoUnit.YEARS.between(datetime, now) >= overrideYearsPassed) {
        if (overrideValue != null && !overrideValue.isEmpty()) {
          return overrideValue;
        }
        return String.valueOf(overrideYearsPassed) + "+";
      }
    }

    // If the input date occurred at least a given number of years ago, adjust the date so it
    // is a given number of years before the current year. Return the updated date or just the
    // year, as per configuration.
    // Otherwise, continue processing.
    if (maxYearsAgoMask) {
      Temporal now = datetimeHasOffset ? ZonedDateTime.now() : LocalDateTime.now();
      if (ChronoUnit.YEARS.between(datetime, now) >= maxYearsAgo) {
        datetime = datetime.minus(shiftFromCurrentYear, ChronoUnit.YEARS);
        if (maxYearsAgoOnlyYear) {
          return String.format("%d", datetime.get(ChronoField.YEAR));
        }
        return f.format(datetime);
      }
    }

    // If the input date occurred at least a given number of days ago, adjust the date so it
    // is the year a given number of days before the current year. Return the updated date or
    // just the year, as per configuration.
    // Otherwise, continue processing.
    if (maxDaysAgoMask) {
      Temporal now = datetimeHasOffset ? ZonedDateTime.now() : LocalDateTime.now();
      if (ChronoUnit.DAYS.between(datetime, now) >= maxDaysAgo) {
        datetime = datetime.minus(shiftFromCurrentDay, ChronoUnit.DAYS);
        if (maxYearsAgoOnlyYear) {
          return String.format("%d", datetime.get(ChronoField.YEAR));
        }
        return f.format(datetime);
      }
    }

    // If the input date occurred after a given number of days ago, return the day and month.
    // Otherwise, continue processing.
    if (yearDeleteNDays) {
      Temporal now = datetimeHasOffset ? ZonedDateTime.now() : LocalDateTime.now();
      if (ChronoUnit.DAYS.between(datetime, now) < yearDeleteNDaysValue) {
        return String.format("%02d/%02d", datetime.get(ChronoField.DAY_OF_MONTH),
            datetime.get(ChronoField.MONTH_OF_YEAR));
      }
    }

    // Return the input date shifted a given, constant number of seconds
    if (shiftDate) {
      datetime = datetime.plus(shiftSeconds, ChronoUnit.SECONDS);
      return f.format(datetime);
    }

    // Return the week and the year
    if (generalizeWeekYear) {
      return String.format("%02d/%d", datetime.get(ChronoField.ALIGNED_DAY_OF_WEEK_IN_YEAR),
          datetime.get(ChronoField.YEAR));
    }

    // Return the month and the year
    if (generalizeMonthYear) {
      return String.format("%02d/%d", datetime.get(ChronoField.MONTH_OF_YEAR),
          datetime.get(ChronoField.YEAR));
    }

    // Return the quarter and the year
    if (generalizeQuarterYear) {
      int quarter = (datetime.get(ChronoField.MONTH_OF_YEAR) / 3) + 1;
      return String.format("%02d/%d", quarter, datetime.get(ChronoField.YEAR));
    }

    // Return the year
    if (generalizeYear) {
      return String.format("%d", datetime.get(ChronoField.YEAR));
    }

    // Return the day and month
    if (yearDelete) {
      return String.format("%02d/%02d", datetime.get(ChronoField.DAY_OF_MONTH),
          datetime.get(ChronoField.MONTH_OF_YEAR));
    }

    // Return the year from the input date modified so that it is not more
    // than 90 years before the current date
    if (generalizeYearMaskAgeOver90) {
      Temporal now = datetimeHasOffset ? ZonedDateTime.now() : LocalDateTime.now();
      if (ChronoUnit.YEARS.between(datetime, now) >= 90) {
        datetime = datetime.minus(90, ChronoUnit.YEARS);
      }
      return String.format("%d", datetime.get(ChronoField.YEAR));
    }

    // Return the month and year from the input date modified so that it is not more
    // than 90 years before the current date
    if (generalizeMonthYearMaskAgeOver90) {
      Temporal now = datetimeHasOffset ? ZonedDateTime.now() : LocalDateTime.now();
      if (ChronoUnit.YEARS.between(datetime, now) >= 90) {
        datetime = datetime.minus(90, ChronoUnit.YEARS);
      }
      return String.format("%02d/%d", datetime.get(ChronoField.MONTH_OF_YEAR),
          datetime.get(ChronoField.YEAR));
    }

    // Randomly modify the year within a given range and continue.
    // This might cause changes to other components of the datetime.
    if (yearMask) {
      int year = datetime.get(ChronoField.YEAR);
      int randomYear = RandomGenerators.randomWithinRange(year, yearRangeDown, yearRangeUp);
      datetime = datetime.plus(randomYear - year, ChronoUnit.YEARS);
    }

    // Randomly modify the month within a given range and continue.
    // This might cause changes to other components of the datetime.
    if (monthMask) {
      int month = datetime.get(ChronoField.MONTH_OF_YEAR);
      int randomMonth = RandomGenerators.randomWithinRange(month, monthRangeDown, monthRangeUp);
      datetime = datetime.plus(randomMonth - month, ChronoUnit.MONTHS);
    }

    // Randomly modify the day of the month within a given range and continue.
    // This might cause changes to other components of the datetime.
    if (dayMask) {
      int day = datetime.get(ChronoField.DAY_OF_MONTH);
      int randomDay = RandomGenerators.randomWithinRange(day, dayRangeDownMin, dayRangeDown,
          dayRangeUpMin, dayRangeUp);
      datetime = datetime.plus(randomDay - day, ChronoUnit.DAYS);
    }

    // Randomly modify the hour within a given range and continue.
    // This might cause changes to other components of the datetime.
    if (hourMask) {
      int hour = datetime.get(ChronoField.HOUR_OF_DAY);
      int randomHour = RandomGenerators.randomWithinRange(hour, hourRangeDown, hourRangeUp);
      datetime = datetime.plus(randomHour - hour, ChronoUnit.HOURS);
    }

    // Randomly modify the minute within a given range and continue.
    // This might cause changes to other components of the datetime.
    if (minutesMask) {
      int minute = datetime.get(ChronoField.MINUTE_OF_HOUR);
      int randomMinutes =
          RandomGenerators.randomWithinRange(minute, minutesRangeDown, minutesRangeUp);
      datetime = datetime.plus(randomMinutes - minute, ChronoUnit.MINUTES);
    }

    // Randomly modify the second within a given range and continue.
    // This might cause changes to other components of the datetime.
    if (secondsMask) {
      int second = datetime.get(ChronoField.SECOND_OF_MINUTE);
      int randomSeconds =
          RandomGenerators.randomWithinRange(second, secondsRangeDown, secondsRangeUp);
      datetime = datetime.plus(randomSeconds - second, ChronoUnit.SECONDS);
    }

    return f.format(datetime);

    // @formatter:off
    /*
    if (dateYearDeleteNInterval) {
      if (dateYearDeleteComparedValue != null) {
        LocalDateTime givenDate = LocalDateTime.ofInstant(cal.toInstant(), ZoneId.systemDefault());
        LocalDateTime comparedDate = null;
        try {
          Instant compareInstant;
          if (f == DateTimeFormatter.ISO_OFFSET_DATE_TIME) {
            compareInstant = Instant.from(f.parse(dateYearDeleteComparedValue));
          } else {
            compareInstant =
                Instant.from(f.withZone(ZoneId.systemDefault()).parse(dateYearDeleteComparedValue));
          }
          comparedDate = LocalDateTime.ofInstant(compareInstant, ZoneId.systemDefault());
        } catch (Exception e) {
          return applyUnexpectedValueHandling(identifier, () -> RandomGenerators
              .generateRandomDate(defaultDateFormat.withZone(ZoneOffset.UTC)));
        }
        long daysBetween = Math.abs(ChronoUnit.DAYS.between(givenDate, comparedDate));
        if (daysBetween <= dateYearDeleteNDaysValue) {
          int originalMonth = cal.get(Calendar.MONTH);
          int originalDay = cal.get(Calendar.DAY_OF_MONTH);
          return String.format("%02d/%02d", originalDay, originalMonth + 1);
        }
      }
    }
    */
    // @formatter:on

    // @formatter:off
    /*
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
    */
    // @formatter:on
  }
}
