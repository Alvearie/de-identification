/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.time.temporal.TemporalAccessor;
import java.util.Arrays;
import java.util.Collection;
import java.util.regex.Pattern;
import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * The type Date time identifier.
 *
 */
public class DateTimeIdentifier extends AbstractIdentifier {
  private static final long serialVersionUID = 2544117023916280649L;

  private static final LogManager log = LogManager.getInstance();

  private static final String[] appropriateNames =
      {"Datetime", "Timestamp", "Birthday", "Birth date", "Date", "BirthDate", "Date of birth"};

  private static final String[] patterns = {"dd-MM-yyyy", "dd-MMM-yyyy", "yyyy-MM-dd", "dd/MM/yyyy",
      "yyyy/MM/dd", "dd-MM-yyyy[ HH:mm:ss]", "yyyy-MM-dd[ HH:mm:ss]", "dd/MM/yyyy[ HH:mm:ss]",
      "yyyy/MM/dd[ HH:mm:ss]"};

  // These patterns are used as a "fast-check" for each of the datetime patterns above.
  private static final Pattern[] datePatterns =
      new Pattern[] {Pattern.compile("^\\d{2}-\\d{2}-\\d{4}$"),
          Pattern.compile("^\\d{2}-.{3,}-\\d{4}$"), Pattern.compile("^\\d{4}-\\d{2}-\\d{2}$"),
          Pattern.compile("^\\d{2}/\\d{2}/\\d{4}$"), Pattern.compile("^\\d{4}/\\d{2}/\\d{2}$"),
          Pattern.compile("^\\d{2}-\\d{2}-\\d{4}( \\d{2}:\\d{2}:\\d{2})?$"),
          Pattern.compile("^\\d{4}-\\d{2}-\\d{2}( \\d{2}:\\d{2}:\\d{2})?$"),
          Pattern.compile("^\\d{2}/\\d{2}/\\d{4}( \\d{2}:\\d{2}:\\d{2})?$"),
          Pattern.compile("^\\d{4}/\\d{2}/\\d{2}( \\d{2}:\\d{2}:\\d{2})?$")};

  private static final DateTimeFormatter dateFormats[] = new DateTimeFormatter[patterns.length];

  static {
    for (int i = 0; i < patterns.length; i++) {
      dateFormats[i] = new DateTimeFormatterBuilder().parseCaseInsensitive()
          .appendPattern((patterns[i]))
          .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
          .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
          .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0).toFormatter();
    }
  }

  public static class DateTimeParseResult {

    private final DateTimeFormatter formatter;
    private final TemporalAccessor accessor;

    public DateTimeParseResult(DateTimeFormatter f, TemporalAccessor a) {
      accessor = a;
      formatter = f;
    }

    public DateTimeFormatter getFormatter() {
      return formatter;
    }

    public TemporalAccessor getValue() {
      return accessor;
    }
  }

  @Override
  public ProviderType getType() {
    return ProviderType.DATETIME;
  }

  /**
   * Parse the given string into a temporal object.
   *
   * @param data the string to parse
   * 
   * @return an object containing the DateTimeFormatter used to parse the string and the temporal
   *         object resulting from the parse or <i>null</i> if no formatters recognized the string.
   */
  public DateTimeParseResult parse(String data) {
    try {
      TemporalAccessor temporalAccessor = DateTimeFormatter.ISO_OFFSET_DATE_TIME.parse(data);
      return new DateTimeParseResult(DateTimeFormatter.ISO_OFFSET_DATE_TIME, temporalAccessor);
    } catch (Exception e) {
      // nothing required here
    }

    for (int i = 0; i < datePatterns.length; i++) {
      if (datePatterns[i].matcher(data).matches()) {
        try {
          DateTimeFormatter f = dateFormats[i];
          TemporalAccessor d = f.parse(data);
          return new DateTimeParseResult(f, d);
        } catch (Exception e) {
          log.logError(LogCodes.WPH1012W, e);
        }
      }
    }

    return null;
  }

  @Override
  public boolean isOfThisType(String data) {
    boolean oftype = false;
    if (data != null) {
      oftype = parse(data.trim()) != null;
    }
    return oftype;
  }

  @Override
  public String getDescription() {
    return "Date and time identification.";
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
