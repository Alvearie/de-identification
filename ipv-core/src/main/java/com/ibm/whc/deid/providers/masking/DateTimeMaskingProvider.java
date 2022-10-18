/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.time.DateTimeException;
import java.time.LocalDateTime;
import java.time.MonthDay;
import java.time.OffsetDateTime;
import java.time.YearMonth;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
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
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

/**
 * Applies privacy protection to date and timestamp values.
 */
public class DateTimeMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -645486689214017719L;

  private static final DateTimeIdentifier dateTimeIdentifier = new DateTimeIdentifier();

  private final boolean maskShiftDate;
  private final int maskShiftSeconds;

  private final boolean generalizeWeekYear;
  private final boolean generalizeMonthYear;
  private final String generalizeMonthYearOutputFormat;
  private final boolean generalizeQuarterYear;
  private final String generalizeQuarterYearOutputFormat;
  private final boolean generalizeYear;

  private final boolean generalizeYearMaskAgeOver90;
  private final boolean generalizeMonthYearMaskAgeOver90;
  private final String generalizeMonthYearMaskAgeOver90OutputFormat;

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

  private final boolean minuteMask;
  private final int minuteRangeUp;
  private final int minuteRangeDown;

  private final boolean secondMask;
  private final int secondRangeUp;
  private final int secondRangeDown;

  private final boolean yearMaxYearsAgoMask;
  private final int yearMaxYearsAgo;
  private final boolean yearMaxYearsAgoOnlyYear;
  private final int yearShiftFromCurrentYear;

  private final boolean dayMaxDaysAgoMask;
  private final int dayMaxDaysAgo;
  private final int dayShiftFromCurrentDay;
  private final boolean dayMaxDaysAgoOnlyYear;

  private final boolean overrideMask;
  private final int overrideYearsPassed;
  private final String overrideValue;
  private final boolean generalizeDayMonth;
  private final String generalizeDayMonthOutputFormat;
  private final boolean yearDeleteNDays;
  private final int yearDeleteNDaysValue;
  private final String yearDeleteNDaysOutputFormat;

  private final String formatFixed;

  public DateTimeMaskingProvider(DateTimeMaskingProviderConfig configuration) {
    super(configuration);

    this.formatFixed = configuration.getFormatFixed();

    this.maskShiftDate = configuration.isMaskShiftDate();
    this.maskShiftSeconds = configuration.getMaskShiftSeconds();

    this.generalizeWeekYear = configuration.isGeneralizeWeekYear();
    this.generalizeMonthYear = configuration.isGeneralizeMonthYear();
    this.generalizeMonthYearOutputFormat = configuration.getGeneralizeMonthYearOutputFormat();
    this.generalizeQuarterYear = configuration.isGeneralizeQuarterYear();
    this.generalizeQuarterYearOutputFormat = configuration.getGeneralizeQuarterYearOutputFormat();
    this.generalizeYear = configuration.isGeneralizeYear();

    this.generalizeYearMaskAgeOver90 = configuration.isGeneralizeYearMaskAgeOver90();
    this.generalizeMonthYearMaskAgeOver90 = configuration.isGeneralizeMonthYearMaskAgeOver90();
    this.generalizeMonthYearMaskAgeOver90OutputFormat =
        configuration.getGeneralizeMonthYearMaskAgeOver90OutputFormat();

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

    this.minuteMask = configuration.isMinuteMask();
    this.minuteRangeUp = configuration.getMinuteRangeUp();
    this.minuteRangeDown = configuration.getMinuteRangeDown();

    this.secondMask = configuration.isSecondMask();
    this.secondRangeUp = configuration.getSecondRangeUp();
    this.secondRangeDown = configuration.getSecondRangeDown();

    this.yearMaxYearsAgoMask = configuration.isYearMaxYearsAgoMask();
    this.yearMaxYearsAgo = configuration.getYearMaxYearsAgo();
    this.yearMaxYearsAgoOnlyYear = configuration.isYearMaxYearsAgoOnlyYear();
    this.yearShiftFromCurrentYear = configuration.getYearShiftFromCurrentYear();

    this.dayMaxDaysAgoMask = configuration.isDayMaxDaysAgoMask();
    this.dayMaxDaysAgo = configuration.getDayMaxDaysAgo();
    this.dayShiftFromCurrentDay = configuration.getDayShiftFromCurrentDay();
    this.dayMaxDaysAgoOnlyYear = configuration.isDayMaxDaysAgoOnlyYear();

    this.overrideMask = configuration.isOverrideMask();
    this.overrideYearsPassed = configuration.getOverrideYearsPassed();
    this.overrideValue = configuration.getOverrideValue();

    this.generalizeDayMonth = configuration.isGeneralizeDayMonth();
    this.generalizeDayMonthOutputFormat = configuration.getGeneralizeDayMonthOutputFormat();
    this.yearDeleteNDays = configuration.isYearDeleteNDays();
    this.yearDeleteNDaysValue = configuration.getYearDeleteNDaysValue();
    this.yearDeleteNDaysOutputFormat = configuration.getYearDeleteNDaysOutputFormat();
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
    String matchedPattern = null;
    boolean patternContainsCaseInsensitiveCharacters = false;

    // note - this can throw IllegalArgumentException, but the pattern has already
    // been checked when the configuration was validated, so this should not occur
    final DateTimeFormatter fixedFormatter =
        DateTimeMaskingProviderConfig.buildOverrideFormatter(formatFixed, null);
    if (fixedFormatter != null) {
      try {
        d = fixedFormatter.parse(identifier);
        f = fixedFormatter;
        matchedPattern = formatFixed;
        // don't apply character case alterations when using custom format
        patternContainsCaseInsensitiveCharacters = false;
      } catch (DateTimeParseException e) {
        return applyUnexpectedValueHandling(identifier,
            () -> RandomGenerators.generateRandomDate(fixedFormatter));
      }
    } else {
      DateTimeParseResult parseResult = dateTimeIdentifier.parse(identifier);
      if (parseResult == null) {
        return applyUnexpectedValueHandling(identifier,
            () -> RandomGenerators.generateRandomDate(DateTimeFormatter.ISO_OFFSET_DATE_TIME));
      }
      f = parseResult.getFormatter();
      d = parseResult.getValue();
      matchedPattern = parseResult.getPattern();
      patternContainsCaseInsensitiveCharacters = parseResult.isVariableCase();
    }

    // keep as much time zone information as available from the original    
    Temporal datetime;
    boolean datetimeHasOffset;
    try {
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
    } catch (DateTimeException e) {
      // The incoming data has been parsed according to one of the formats, but
      // it was not possible to obtain a LocalDateTime from it. Since a time
      // component is defaulted when all the formatters are built, the most
      // likely cause is a custom format did not provide enough information
      // to obtain a year, month, and date. This should be considered an
      // invalid configuration and processing should not have started. However,
      // there is no feasible way to test a custom input pattern without trying
      // real input data to catch these errors earlier. The best alternative
      // is to stop processing now.
      // NOTE - the DateTimeException probably contains data from the input
      // and should not be logged
      String msg = Messages.getMessage(LogCodes.WPH1025W, matchedPattern);
      StringBuilder buffer = new StringBuilder(msg.length() + LogCodes.WPH1025W.length() + 1);
      buffer.append(LogCodes.WPH1025W).append(' ').append(msg);
      throw new IllegalArgumentException(buffer.toString());
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
        if (dayMaxDaysAgoOnlyYear) {
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
        MonthDay monthDay = MonthDay.of(datetime.get(ChronoField.MONTH_OF_YEAR),
            datetime.get(ChronoField.DAY_OF_MONTH));
        DateTimeFormatter outputFormatter = DateTimeMaskingProviderConfig
            .buildOverrideFormatter(yearDeleteNDaysOutputFormat, monthDay);
        if (outputFormatter != null) {
          return outputFormatter.format(monthDay);
        }
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
      // Note - DateTimeFormatter with pattern ww doesn't work as expected for week 53
      return String.format("%02d/%d", datetime.get(ChronoField.ALIGNED_WEEK_OF_YEAR),
          datetime.get(ChronoField.YEAR));
    }

    // Return the month and the year
    if (generalizeMonthYear) {
      int year = datetime.get(ChronoField.YEAR);
      int month = datetime.get(ChronoField.MONTH_OF_YEAR);
      YearMonth yearMonth = YearMonth.of(year, month);
      DateTimeFormatter outputFormatter = DateTimeMaskingProviderConfig
          .buildOverrideFormatter(generalizeMonthYearOutputFormat, yearMonth);
      if (outputFormatter != null) {
        return outputFormatter.format(yearMonth);
      }
      return String.format("%02d/%d", month, year);
    }

    // Return the quarter and the year
    if (generalizeQuarterYear) {
      YearMonth yearMonth =
          YearMonth.of(datetime.get(ChronoField.YEAR), datetime.get(ChronoField.MONTH_OF_YEAR));
      DateTimeFormatter formatter = DateTimeMaskingProviderConfig
          .buildOverrideFormatter(generalizeQuarterYearOutputFormat, yearMonth);
      if (formatter == null) {
        final String PATTERN = "Q/yyyy";
        formatter = DateTimeMaskingProviderConfig.buildOverrideFormatter(PATTERN, null);
      }
      return formatter.format(yearMonth);
    }

    // Return the year
    if (generalizeYear) {
      return String.format("%d", datetime.get(ChronoField.YEAR));
    }

    // Return the day and month
    if (generalizeDayMonth) {
      int month = datetime.get(ChronoField.MONTH_OF_YEAR);
      int day = datetime.get(ChronoField.DAY_OF_MONTH);
      MonthDay monthDay = MonthDay.of(month, day);
      DateTimeFormatter outputFormatter =
          DateTimeMaskingProviderConfig.buildOverrideFormatter(generalizeDayMonthOutputFormat, monthDay);
      if (outputFormatter != null) {
        return outputFormatter.format(monthDay);
      }
      return String.format("%02d/%02d", day, month);
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
      int year = ChronoUnit.YEARS.between(datetime, now) >= 90
          ? now.minus(90, ChronoUnit.YEARS).get(ChronoField.YEAR)
          : datetime.get(ChronoField.YEAR);
      int month = datetime.get(ChronoField.MONTH_OF_YEAR);
      YearMonth yearMonth = YearMonth.of(year, month);
      DateTimeFormatter outputFormatter = DateTimeMaskingProviderConfig
          .buildOverrideFormatter(generalizeMonthYearMaskAgeOver90OutputFormat, yearMonth);
      if (outputFormatter != null) {
        return outputFormatter.format(yearMonth);
      }
      return String.format("%02d/%d", month, year);
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
    if (minuteMask) {
      int minute = datetime.get(ChronoField.MINUTE_OF_HOUR);
      int randomMinutes =
          RandomGenerators.randomWithinRange(minute, minuteRangeDown, minuteRangeUp);
      datetime = datetime.plus(randomMinutes - minute, ChronoUnit.MINUTES);
    }

    // Randomly modify the second within a given range and continue.
    // This might cause changes to other components of the datetime.
    if (secondMask) {
      int second = datetime.get(ChronoField.SECOND_OF_MINUTE);
      int randomSeconds =
          RandomGenerators.randomWithinRange(second, secondRangeDown, secondRangeUp);
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
