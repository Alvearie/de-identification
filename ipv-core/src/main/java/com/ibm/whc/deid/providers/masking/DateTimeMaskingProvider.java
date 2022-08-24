/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.temporal.Temporal;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalQueries;
import com.ibm.whc.deid.providers.identifiers.DateTimeIdentifier;
import com.ibm.whc.deid.providers.identifiers.DateTimeIdentifier.DateTimeParseResult;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeMaskingProviderConfig;
import com.ibm.whc.deid.util.RandomGenerators;

/**
 * Applies privacy protection to datetime values.
 */
public class DateTimeMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -645486689214017719L;

  private static final DateTimeIdentifier dateTimeIdentifier = new DateTimeIdentifier();

  private final boolean maskShiftDate;
  private final int maskShiftSeconds;

  private final boolean generalizeWeekYear;
  private final boolean generalizeMonthYear;
  private final boolean generalizeQuarterYear;
  private final boolean generalizeYear;

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

  private final boolean yearMaxYearsAgoMask;
  private final int yearMaxYearsAgo;
  private final boolean yearMaxYearsAgoOnlyYear;
  private final int yearShiftFromCurrentYear;

  private final boolean dayMaxDaysAgoMask;
  private final int dayMaxDaysAgo;
  private final int dayShiftFromCurrentDay;

  private final boolean overrideMask;
  private final int overrideYearsPassed;
  private final String overrideValue;
  private final boolean yearDelete;
  private final boolean yearDeleteNDays;
  private final int yearDeleteNDaysValue;
  private final String fixedFormatString;

  public DateTimeMaskingProvider(DateTimeMaskingProviderConfig configuration) {
    super(configuration);

    this.fixedFormatString = configuration.getFormatFixed();

    this.maskShiftDate = configuration.isMaskShiftDate();
    this.maskShiftSeconds = configuration.getMaskShiftSeconds();

    this.generalizeWeekYear = configuration.isGeneralizeWeekyear();
    this.generalizeMonthYear = configuration.isGeneralizeMonthyear();
    this.generalizeQuarterYear = configuration.isGeneralizeQuarteryear();
    this.generalizeYear = configuration.isGeneralizeYear();

    this.generalizeYearMaskAgeOver90 = configuration.isGeneralizeYearMaskAgeOver90();
    this.generalizeMonthYearMaskAgeOver90 = configuration.isGeneralizeMonthyearMaskAgeOver90();

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

    this.yearMaxYearsAgoMask = configuration.isYearMaxYearsAgoMask();
    this.yearMaxYearsAgo = configuration.getYearMaxYearsAgo();
    this.yearMaxYearsAgoOnlyYear = configuration.isYearMaxYearsAgoOnlyYear();
    this.yearShiftFromCurrentYear = configuration.getYearShiftFromCurrentYear();
    this.dayMaxDaysAgoMask = configuration.isDayMaxDaysAgoMask();
    this.dayMaxDaysAgo = configuration.getDayMaxDaysAgo();
    this.dayShiftFromCurrentDay = configuration.getDayShiftFromCurrentDay();

    this.overrideMask = configuration.isOverrideMask();
    this.overrideYearsPassed = configuration.getOverrideYearsPassed();
    this.overrideValue = configuration.getOverrideValue();

    this.yearDelete = configuration.isYearDelete();
    this.yearDeleteNDays = configuration.isYearDeleteNdays();
    this.yearDeleteNDaysValue = configuration.getYearDeleteNdaysValue();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    boolean isAllLowerCase = identifier.equals(identifier.toLowerCase());
    boolean isAllUpperCase = identifier.equals(identifier.toUpperCase());

    DateTimeFormatter f = null;
    TemporalAccessor d = null;
    boolean patternContainsCaseInsensitiveCharacters = false;

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
          // don't apply character case alterations when using custom format
          patternContainsCaseInsensitiveCharacters = false;
        } catch (DateTimeParseException e) {
          return applyUnexpectedValueHandling(identifier,
              () -> RandomGenerators.generateRandomDate(fixedFormatter));
        }
      } catch (IllegalArgumentException e) {
        // thrown if the pattern is not a valid datetime pattern
        return applyUnexpectedValueHandling(identifier,
            () -> RandomGenerators.generateRandomDate(DateTimeFormatter.ISO_OFFSET_DATE_TIME));
      }

    } else {
      DateTimeParseResult parseResult = dateTimeIdentifier.parse(identifier);
      if (parseResult == null) {
        return applyUnexpectedValueHandling(identifier,
            () -> RandomGenerators.generateRandomDate(DateTimeFormatter.ISO_OFFSET_DATE_TIME));
      }
      f = parseResult.getFormatter();
      d = parseResult.getValue();
      patternContainsCaseInsensitiveCharacters = parseResult.isVariableCase();
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

    // Subtract the given number of years from the current year and change the input date to that
    // year, if the input date is at least a given number of years ago.
    // Return the updated date or just the year, as per configuration.
    // Otherwise, continue processing.
    if (yearMaxYearsAgoMask) {
      Temporal now = datetimeHasOffset ? ZonedDateTime.now() : LocalDateTime.now();
      if (ChronoUnit.YEARS.between(datetime, now) >= yearMaxYearsAgo) {
        // Get the new year value by subtracting the configured amount from the current year
        now = now.minus(yearShiftFromCurrentYear, ChronoUnit.YEARS);
        int newyear = now.get(ChronoField.YEAR);
        if (yearMaxYearsAgoOnlyYear) {
          return String.format("%d", newyear);
        }
        datetime = datetime.with(ChronoField.YEAR, newyear);
        String result = f.format(datetime);
        return applyCharacterCase(patternContainsCaseInsensitiveCharacters, result, isAllUpperCase,
            isAllLowerCase);
      }
    }

    // Subtract the given number of days from the current date and change the input date the
    // resulting year, if the input date is at least a given number of days ago.
    // Return the updated date or just the year, as per configuration.
    // Otherwise, continue processing.
    if (dayMaxDaysAgoMask) {
      Temporal now = datetimeHasOffset ? ZonedDateTime.now() : LocalDateTime.now();
      if (ChronoUnit.DAYS.between(datetime, now) >= dayMaxDaysAgo) {
        now = now.minus(dayShiftFromCurrentDay, ChronoUnit.DAYS);
        int newyear = now.get(ChronoField.YEAR);
        if (yearMaxYearsAgoOnlyYear) {
          return String.format("%d", newyear);
        }
        datetime = datetime.with(ChronoField.YEAR, newyear);
        String result = f.format(datetime);
        return applyCharacterCase(patternContainsCaseInsensitiveCharacters, result, isAllUpperCase,
            isAllLowerCase);
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
    if (maskShiftDate) {
      datetime = datetime.plus(maskShiftSeconds, ChronoUnit.SECONDS);
      String result = f.format(datetime);
      return applyCharacterCase(patternContainsCaseInsensitiveCharacters, result, isAllUpperCase,
          isAllLowerCase);
    }

    // Return the week and the year
    if (generalizeWeekYear) {
      return String.format("%02d/%d", datetime.get(ChronoField.ALIGNED_WEEK_OF_YEAR),
          datetime.get(ChronoField.YEAR));
    }

    // Return the month and the year
    if (generalizeMonthYear) {
      return String.format("%02d/%d", datetime.get(ChronoField.MONTH_OF_YEAR),
          datetime.get(ChronoField.YEAR));
    }

    // Return the quarter and the year
    if (generalizeQuarterYear) {
      int month = datetime.get(ChronoField.MONTH_OF_YEAR);
      int quarter = (month / 3);
      if (month % 3 > 0) {
        quarter++;
      }
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
      // @formatter:off
      Temporal dateToReturn = ChronoUnit.YEARS.between(datetime, now) >= 90 
              ? now.minus(90, ChronoUnit.YEARS)
              : datetime;
      // @formatter:on
      return String.format("%d", dateToReturn.get(ChronoField.YEAR));
    }

    // Return the month and year from the input date modified so that it is not more
    // than 90 years before the current date
    if (generalizeMonthYearMaskAgeOver90) {
      Temporal now = datetimeHasOffset ? ZonedDateTime.now() : LocalDateTime.now();
      // @formatter:off
      Temporal dateToReturn = ChronoUnit.YEARS.between(datetime, now) >= 90 
              ? now.minus(90, ChronoUnit.YEARS)
              : datetime;
      // @formatter:on
      return String.format("%02d/%d", dateToReturn.get(ChronoField.MONTH_OF_YEAR),
          dateToReturn.get(ChronoField.YEAR));
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

    String result = f.format(datetime);
    return applyCharacterCase(patternContainsCaseInsensitiveCharacters, result, isAllUpperCase,
        isAllLowerCase);
  }

  protected String applyCharacterCase(boolean patternContainsCaseInsensitveChars, String datetime,
      boolean wasAllUpperCase, boolean wasAllLowerCase) {
    String result = datetime;
    if (patternContainsCaseInsensitveChars) {
      if (wasAllUpperCase) {
        result = datetime.toUpperCase();
      } else if (wasAllLowerCase) {
        result = datetime.toLowerCase();
      }
    }
    return result;
  }
}
