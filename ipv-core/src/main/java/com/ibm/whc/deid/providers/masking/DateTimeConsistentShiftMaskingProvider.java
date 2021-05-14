/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.time.temporal.Temporal;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalQueries;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;
import java.util.regex.Pattern;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.WordUtils;
import com.fasterxml.jackson.databind.JsonNode;
import com.ibm.whc.deid.jsonpath.JSONPath;
import com.ibm.whc.deid.providers.masking.fhir.MaskingActionInputIdentifier;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeConsistentShiftMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeConsistentShiftMaskingProviderConfig.DateShiftDirection;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import com.ibm.whc.deid.util.HashUtils;
import com.ibm.whc.deid.util.Tuple;
import com.ibm.whc.deid.utils.log.LogCodes;

public class DateTimeConsistentShiftMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -5672151364906956472L;

  // used to "test" input for suitability for a formatter before attempting to
  // parse with that formatter - reduces the expense of creating exceptions related
  // to input-to-pattern mismatches
  private static final Pattern[] datePatterns =
      new Pattern[] {Pattern.compile("^\\d{2}-\\w{3}-\\d{4}$"),
          Pattern.compile("^\\d{4}-\\d{2}-\\d{2}$"), Pattern.compile("^\\d{4}/\\d{2}/\\d{2}$"),
          Pattern.compile("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"),
          Pattern.compile("^\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}:\\d{2}$"),
          Pattern.compile("^\\d{2}-\\d{2}-\\d{4}$"), Pattern.compile("^\\d{2}/\\d{2}/\\d{4}$"),
          Pattern.compile("^\\d{2}-\\d{2}-\\d{4} \\d{2}:\\d{2}:\\d{2}$"),
          Pattern.compile("^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}$")};

  private static final String[] dateFormatStrings =
      {"dd-MMM-yyyy", "yyyy-MM-dd", "yyyy/MM/dd", "yyyy-MM-dd HH:mm:ss", "yyyy/MM/dd HH:mm:ss",
          "dd-MM-yyyy", "dd/MM/yyyy", "dd-MM-yyyy HH:mm:ss", "dd/MM/yyyy HH:mm:ss"};

  private static final DateTimeFormatter dateFormaters[] =
      new DateTimeFormatter[dateFormatStrings.length];

  static {
    for (int i = 0; i < dateFormatStrings.length; i++) {
      dateFormaters[i] =
          new DateTimeFormatterBuilder().appendPattern((dateFormatStrings[i])).toFormatter();
      // .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
      // .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
      // .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0).toFormatter();
    }
  }

  private final int dateShiftMinimumDays;
  private final int dateShiftMaximumDays;
  private final DateShiftDirection dateShiftDirection;
  private final List<DateTimeFormatter> customFormatters;
  private final JSONPath compiledPathExp;

  public DateTimeConsistentShiftMaskingProvider(
      DateTimeConsistentShiftMaskingProviderConfig configuration, DeidMaskingConfig deidMaskingConfig) {
    super(configuration);
    // ensure config validation is called - should already have been
    try {
      configuration.validate(deidMaskingConfig);
    } catch (InvalidMaskingConfigurationException e) {
      throw new RuntimeException(e);
    }
    
    this.dateShiftMinimumDays = configuration.getDateShiftMinimumDays();
    this.dateShiftMaximumDays = configuration.getDateShiftMaximumDays();
    this.dateShiftDirection = configuration.getDateShiftDirection();

    List<DateTimeFormatter> formatters = null;
    List<String> formats = configuration.getCustomFormats();
    if (formats != null) {
      formatters = new ArrayList<>(formats.size());
      for (String format : formats) {
        if (format != null && !format.trim().isEmpty()) {
          formatters.add(new DateTimeFormatterBuilder().appendPattern(format).toFormatter());
        }
      }
    }
    this.customFormatters = formatters;

    // path must start with '/' as required for JsonPointer
    String path = configuration.getPatientIdentifierPath().trim();
    if (path.charAt(0) != '/') {
      path = "/" + path;
    }
    this.compiledPathExp = JSONPath.compile(path);
  }

  @Override
  public String mask(String identifier) {
    throw new NotImplementedException();
  }

  @Override
  public void maskIdentifierBatch(List<MaskingActionInputIdentifier> identifiers) {
    for (MaskingActionInputIdentifier maskingActionInputIdentifier : identifiers) {
      String replacementValue = generateReplacement(maskingActionInputIdentifier);
      // set the replacement value into the record
      putField(maskingActionInputIdentifier, replacementValue);
    }
  }
  
  protected String generateReplacement(MaskingActionInputIdentifier maskingActionInputIdentifier) {
    String originalValue = maskingActionInputIdentifier.getNode().asText();
    if (originalValue == null || originalValue.trim().isEmpty()) {
      return applyUnexpectedValueHandling(String.valueOf(originalValue), null);
    }

    String patientId = getPatientIdentifier(maskingActionInputIdentifier);
    if (patientId == null || patientId.trim().isEmpty()) {
      return applyUnexpectedValueHandling("patient identifier `" + patientId + "`", null);
    }

    int offsetDays = generateShiftNumberOfDays(patientId);

    return applyOffsetAndReformat(originalValue, offsetDays);
  }
  
  /**
   * Retrieves the value of the property at the configured patient identifier path starting from the
   * root JSON node provided by the given masking operation.
   * 
   * @param identifier the masking operation that provides access to the root of the JSON document
   * 
   * @return the value at the configured path or an empty string if the configured path is not found
   *         in the document or the field at that path is null
   */
  protected String getPatientIdentifier(MaskingActionInputIdentifier identifier) {
    JsonNode node = this.compiledPathExp.apply(identifier.getRoot());
    // use default value in asText() to ensure "missing" nodes and explicit
    // null nodes are treated as empty string
    String patientId = node.asText("");
    return patientId;
  }

  /**
   * Generates the number of days, which might be positive or negative depending on configuration,
   * dates should be shifted for the given patient identifier. The same number is returned every
   * time the same patient identifier is presented, unless the configured ranges for possible
   * offsets are changed.
   * 
   * @param patientId the non-null patient identifier
   * 
   * @return the number of days to shift target dates
   */
  protected int generateShiftNumberOfDays(String patientId) {

    // determine the total number of eligible numbers of days based on the configuration
    int rangeLength = this.dateShiftMaximumDays - this.dateShiftMinimumDays + 1;
    int numberOfPossibleShiftValues = rangeLength;
    if (this.dateShiftDirection == DateShiftDirection.beforeOrAfter) {
      numberOfPossibleShiftValues *= 2;
      if (this.dateShiftMinimumDays == 0) {
        numberOfPossibleShiftValues--; // don't add 0 twice
      }
    }

    // use the patient identifier to generate an index into the possible values
    long seedLong = generateLongFromString(patientId);
    int possiblesIndex = (int) (seedLong % numberOfPossibleShiftValues);

    // get the value at the index into the total possible values
    int shfitNumberOfDays = -1;
    switch (this.dateShiftDirection) {
      case before:
        shfitNumberOfDays = -this.dateShiftMaximumDays + possiblesIndex;
        break;
      case after:
        shfitNumberOfDays = this.dateShiftMinimumDays + possiblesIndex;
        break;
      default:
        if (possiblesIndex < rangeLength) {
          shfitNumberOfDays = -this.dateShiftMaximumDays + possiblesIndex;
        } else {
          possiblesIndex -= rangeLength;
          if (this.dateShiftMinimumDays == 0) {
            // skip 0 because it would have been counted already as part of the "before" range
            possiblesIndex++;
          }
          shfitNumberOfDays = this.dateShiftMinimumDays + possiblesIndex;
        }
    }

    return shfitNumberOfDays;
  }

  /**
   * Generate a repeatable long value from a given string. Each time the same string is presented,
   * the same long is returned.
   * 
   * <p>
   * Note that there is no guarantee that the same long is not returned for various different
   * strings, only that the same string will return the same long. In particular leading and
   * trailing whitespace and differences in character case do not alter the long values generated
   * for different strings. This is a convenience so that minor alterations in the input data do not
   * adversely impact results. For example, the same long is generated for the input values
   * "patient1", "patient1 ", "Patient1".
   * 
   * @param seed the string from which a long numeric values is to be generated
   * 
   * @return the generated numeric value
   */
  protected long generateLongFromString(String seed) {
    // a null value is not expected, but empty string is an appropriate way to handle
    if (seed == null) {
      seed = "";
    } else {
      seed = seed.trim().toUpperCase();
    }

    long generatedLong = HashUtils.longFromHash(seed);

    // NOTE - Math.abs(long) returns negative number if given long is MIN_VALUE
    if (generatedLong == Long.MIN_VALUE) {
      generatedLong++;
    }
    return Math.abs(generatedLong);
  }

  /**
   * Shift the date or date-time value represented by the given string the given number of days and
   * reformat the result back into the same format as the given string.
   * 
   * @param originalValue a date or date and time value in string form
   * 
   * @param offsetDays the number of days the temporal value should be shifted, which might be
   *        positive, negative, or zero.
   * 
   * @return the shifted temporal value in the same string format as the given value
   */
  protected String applyOffsetAndReformat(String originalValue, int offsetDays) {

    Tuple<DateTimeFormatter, TemporalAccessor> parseResponse = parse(originalValue);
    if (parseResponse == null) {
      return applyUnexpectedValueHandling(originalValue, null);
    }
    DateTimeFormatter formatter = parseResponse.getFirst();
    TemporalAccessor parsedOriginal = parseResponse.getSecond();

    Temporal adjustedTemporal = adjust(parsedOriginal, offsetDays);

    String replacement = formatter.format(adjustedTemporal);
    return replacement;
  }

  /**
   * Parse the given date or date and time string into a temporal object and return that object
   * along with the formatter that recognized and converted it.
   * 
   * @param originalValue a date or date and time value in string form
   * 
   * @return a "tuple" object containing the formatter that recognized the form of the string value
   *         (first member) and the temporal object creatd by parsing the string (second member) or
   *         <i>null</i> if the given string was not recognized as a supported format
   */
  protected Tuple<DateTimeFormatter, TemporalAccessor> parse(String originalValue) {
    Tuple<DateTimeFormatter, TemporalAccessor> tuple = null;

    if (this.customFormatters != null) {
      for (DateTimeFormatter formatter : this.customFormatters) {
        try {
          TemporalAccessor accessor = formatter.parse(originalValue);
          tuple = new Tuple<>(formatter, accessor);
          break;
        } catch (Exception e) {
          // continue processing
        }
      }
    }

    if (tuple == null) {
      // try the ISO date-time with offset format first
      try {
        TemporalAccessor accessor = DateTimeFormatter.ISO_OFFSET_DATE_TIME.parse(originalValue);
        tuple = new Tuple<>(DateTimeFormatter.ISO_OFFSET_DATE_TIME, accessor);
      } catch (Exception e) {
        // continue processing
      }
    }

    // try the builtin formats that do not include time zones next
    if (tuple == null) {
      // if using alphabetics, camelcase the month, otherwise it will not be recognized
      String capitalizedValue = originalValue.matches(".*[a-zA-Z].*")
          ? WordUtils.capitalizeFully(originalValue, new char[] {'-'})
          : originalValue;
      for (int i = 0; i < datePatterns.length; i++) {
        Pattern p = datePatterns[i];
        if (p.matcher(capitalizedValue).matches()) {
          try {
            DateTimeFormatter f = dateFormaters[i];
            TemporalAccessor d = f.parse(capitalizedValue);
            tuple = new Tuple<>(f, d);
            break;
          } catch (Exception e2) {
            log.logError(LogCodes.WPH1013E, e2);
          }
        }
      }
    }

    return tuple;
  }

  /**
   * Creates a new temporal object that is the given number of days different than the given
   * temporal object.
   * 
   * @param original the original date or date and time value
   * 
   * @param offsetDays the number of days to change the value, which might be positive, negative, or
   *        zero
   * 
   * @return the adjusted temporal object
   */
  protected Temporal adjust(TemporalAccessor original, int offsetDays) {
    Temporal adjusted;
    ZoneId originalZoneId = original.query(TemporalQueries.zone());
    if (originalZoneId != null) {
      // If the given value has offset from UTC information, use a Calendar to manipulate
      // the number of days in case the offset information also includes daylight savings
      // time rules. This is important in case the time, which is normally not changed,
      // needs to change to account for days that do not have a standard 24 hours due to
      // daylight savings time. For example, if the original date had 24 hours and the
      // target date has only 23, the original time might represent a time that did not
      // actually occur on the target date. Returning valid, possible times for the
      // new date is desired when the original string contains enough information to
      // accomplish this.
      Instant originalInstant = Instant.from(original);
      long epochMillis = originalInstant.toEpochMilli();
      Calendar cal = Calendar.getInstance(TimeZone.getTimeZone(originalZoneId));
      cal.setTimeInMillis(epochMillis);
      cal.add(Calendar.DAY_OF_MONTH, offsetDays);
      // adjusted = OffsetDateTime.ofInstant(cal.toInstant(), originalZoneId);
      adjusted = ZonedDateTime.ofInstant(cal.toInstant(), originalZoneId);
    } else if (original.isSupported(ChronoField.SECOND_OF_DAY)) {
      // No time zone offset information is available, but a time portion is provided
      LocalDateTime originalAsTemporal = LocalDateTime.from(original);
      adjusted = originalAsTemporal.plusDays(offsetDays);
    } else {
      // The given value contains only a date
      LocalDate originalAsTemporal = LocalDate.from(original);
      adjusted = originalAsTemporal.plusDays(offsetDays);
    }
    return adjusted;
  }
}
