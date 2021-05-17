/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import java.time.Instant;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoField;
import java.time.temporal.TemporalAccessor;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.regex.Pattern;
import org.apache.commons.lang.WordUtils;
import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.util.Tuple;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * The type Date time identifier.
 *
 */
public class DateTimeIdentifier extends AbstractIdentifier {
  private static final long serialVersionUID = 2544117023916280649L;

  private static final String[] appropriateNames =
      {"Datetime", "Timestamp", "Birthday", "Birth date", "Date", "BirthDate", "Date of birth"};

  private static final String[] patterns = {"dd-MM-yyyy", "dd-MMM-yyyy", "yyyy-MM-dd", "dd/MM/yyyy",
      "yyyy/MM/dd", "dd-MM-yyyy[ HH:mm:ss]", "yyyy-MM-dd[ HH:mm:ss]", "dd/MM/yyyy[ HH:mm:ss]",
      "yyyy/MM/dd[ HH:mm:ss]"};

  private static final Pattern[] datePatterns =
      new Pattern[] {Pattern.compile("^\\d{2}-\\d{2}-\\d{4}$"),
          Pattern.compile("^\\d{2}-\\w{3}-\\d{4}$"), Pattern.compile("^\\d{4}-\\d{2}-\\d{2}$"),
          Pattern.compile("^\\d{2}/\\d{2}/\\d{4}$"), Pattern.compile("^\\d{4}/\\d{2}/\\d{2}$"),
          Pattern.compile("^\\d{2}-\\d{2}-\\d{4}( \\d{2}:\\d{2}:\\d{2})?$"),
          Pattern.compile("^\\d{4}-\\d{2}-\\d{2}( \\d{2}:\\d{2}:\\d{2})?$"),
          Pattern.compile("^\\d{2}/\\d{2}/\\d{4}( \\d{2}:\\d{2}:\\d{2})?$"),
          Pattern.compile("^\\d{4}/\\d{2}/\\d{2}( \\d{2}:\\d{2}:\\d{2})?$")};

  private static final DateTimeFormatter dateFormats[] = new DateTimeFormatter[patterns.length + 1];

  private static final LogManager log = LogManager.getInstance();

  static {
    dateFormats[patterns.length] = DateTimeFormatter.ISO_INSTANT;
    for (int i = 0; i < patterns.length; i++) {
      dateFormats[i] = new DateTimeFormatterBuilder().appendPattern((patterns[i]))
          .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
          .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
          .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0).toFormatter().withZone(ZoneOffset.UTC);
    }
  }

  @Override
  public ProviderType getType() {
    return ProviderType.DATETIME;
  }

  /**
   * Lazy match boolean.
   *
   * @param data the data
   * @return the boolean
   */
  public boolean lazyMatch(String data) {
    try {
      Instant.parse(data);
      return true;
    } catch (DateTimeParseException e) {
      // If it can't parse, then carry on.
    }
    for (Pattern p : datePatterns) {
      if (p.matcher(data).matches()) {
        return true;
      }
    }

    return false;
  }

  /**
   * Matching format date format.
   *
   * @param data the data
   * @return the date format
   */
  public DateTimeFormatter matchingFormat(String data) {
    try {
      Instant.parse(data);
      return DateTimeFormatter.ISO_OFFSET_DATE_TIME;

    } catch (DateTimeParseException e) {
      // If it can't parse, then carry on.
    }
    for (int i = 0; i < datePatterns.length; i++) {
      Pattern p = datePatterns[i];
      if (p.matcher(data).matches()) {
        try {
          DateTimeFormatter f = dateFormats[i];
          f.parse(data);
          return f;
        } catch (Exception ignored) {
        }
      }
    }

    return null;
  }

  /**
   * Parse tuple.
   *
   * @param data the data
   * @return the tuple
   */
  public Tuple<DateTimeFormatter, Date> parse(String data) {
    try {
      TemporalAccessor current = DateTimeFormatter.ISO_OFFSET_DATE_TIME.parse(data);
      return new Tuple<>(DateTimeFormatter.ISO_OFFSET_DATE_TIME, Date.from(Instant.from(current)));
    } catch (Exception e) {
    }

    // if there using alphabets, we need to camelcase the month, otherwise
    // it will not be recognized
    if (data.matches(".*[a-zA-Z].*")) {
      data = WordUtils.capitalizeFully(data, new char[] {'-'});
    }

    for (int i = 0; i < datePatterns.length; i++) {
      Pattern p = datePatterns[i];
      if (p.matcher(data).matches()) {
        try {
          DateTimeFormatter f = dateFormats[i];
          TemporalAccessor d = f.withZone(ZoneId.systemDefault()).parse(data);
          return new Tuple<>(f, Date.from(Instant.from(d)));
        } catch (Exception e) {
          log.logError(LogCodes.WPH1013E, e);
        }
      }
    }

    return null;
  }

  @Override
  public boolean isOfThisType(String data) {
    return matchingFormat(data.trim()) != null;
  }

  @Override
  public String getDescription() {
    return "Date and time identification. Formats recognized are dd-MM-yyyy HH:mm:ss and dd/MM/yyyy HH:mm:ss";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.DATE;
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }
}
