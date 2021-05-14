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
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;
import java.util.regex.Pattern;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.WordUtils;
import com.fasterxml.jackson.databind.JsonNode;
import com.ibm.whc.deid.jsonpath.JSONPath;
import com.ibm.whc.deid.providers.masking.fhir.MaskingActionInputIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeConsistentShiftMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeConsistentShiftMaskingProviderConfig.DateShiftDirection;
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
          Pattern.compile("^\\d{4}-\\d{2}-\\d{2}( \\d{2}:\\d{2}:\\d{2})?$"),
          Pattern.compile("^\\d{4}/\\d{2}/\\d{2}( \\d{2}:\\d{2}:\\d{2})?$")};

  private static final String[] dateFormatStrings =
      {"dd-MMM-yyyy", "yyyy-MM-dd", "yyyy/MM/dd", "yyyy-MM-dd[ HH:mm:ss]", "yyyy/MM/dd[ HH:mm:ss]"};

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
  private final List<String> overrideFormats;
  private final JSONPath compiledPathExp;

  public DateTimeConsistentShiftMaskingProvider(
      DateTimeConsistentShiftMaskingProviderConfig configuration) {
    super(configuration);
    this.dateShiftMinimumDays = configuration.getDateShiftMinimumDays();
    this.dateShiftMaximumDays = configuration.getDateShiftMaximumDays();
    this.dateShiftDirection = configuration.getDateShiftDirection();
    this.overrideFormats = configuration.getCustomFormats();

    // path must start with '/' as required for JsonPointer
    String path = configuration.getPatientIdentifierPath().trim();
    if (path.charAt(0) != '/') {
      path = "/" + path;
    }
    this.compiledPathExp = JSONPath.compile(path);

    // Get the replacement provider
    // this.dateTimeMaskingProvider =
    // maskingProviderFactory.getProviderFromType(MaskingProviderType.DATETIME,
    // deidMaskingConfig, dateTimeProviderConfig, tenantId, localizationProperty);
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
      return applyUnexpectedValueHandling(originalValue, null);
    }

    int offsetDays = generateShiftNumberOfDays(patientId);

    return applyOffsetAndReformat(originalValue, offsetDays);
  }
  
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
    // Remove any wrapping whitespace and convert to a common case so that
    // minor differences in how the patient identifier is written will not change the offset.
    // For example, "patient", "patient ", "Patient" all generate the same offset.
    String seed = patientId.trim().toUpperCase();

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
    long seedLong = generateLongFromString(seed);
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

  protected long generateLongFromString(String seed) {
    long generatedLong = HashUtils.longFromHash(seed);
    // NOTE - Math.abs(long) returns negative number if given long is MIN_VALUE
    if (generatedLong == Long.MIN_VALUE) {
      generatedLong++;
    }
    return Math.abs(generatedLong);
  }

  protected String applyOffsetAndReformat(String originalValue, int offsetDays) {
    // TODO: add format override?
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

  protected Tuple<DateTimeFormatter, TemporalAccessor> parse(String originalValue) {
    Tuple<DateTimeFormatter, TemporalAccessor> tuple = null;
    try {
      TemporalAccessor accessor = DateTimeFormatter.ISO_OFFSET_DATE_TIME.parse(originalValue);
      tuple = new Tuple<>(DateTimeFormatter.ISO_OFFSET_DATE_TIME, accessor);
    } catch (Exception e) {
      // continue processing
    }

    if (tuple == null) {
      // if using alphabetics, camelcase the month, otherwise it will not be recognized
      String capitalizedValue = originalValue.matches(".*[a-zA-Z].*") ?
          WordUtils.capitalizeFully(originalValue, new char[] {'-'}) : originalValue;
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
    
    // TODO: leave in?
    if (tuple == null) {
      try {
        TemporalAccessor accessor = DateTimeFormatter.ISO_ZONED_DATE_TIME.parse(originalValue);
        tuple = new Tuple<>(DateTimeFormatter.ISO_ZONED_DATE_TIME, accessor);
      } catch (Exception e) {
        // continue processing
      }
    }

    return tuple;
  }

  protected Temporal adjust(TemporalAccessor original, int offsetDays) {
    Temporal adjusted;
    if (original.isSupported(ChronoField.OFFSET_SECONDS)) {
      Instant originalInstant = Instant.from(original);
      ZoneId originalZoneId = ZoneId.from(original);
      long epochMillis = originalInstant.toEpochMilli();
      Calendar cal = Calendar.getInstance(TimeZone.getTimeZone(originalZoneId));
      cal.setTimeInMillis(epochMillis);
      cal.add(Calendar.DAY_OF_MONTH, offsetDays);
      // adjusted = OffsetDateTime.ofInstant(cal.toInstant(), originalZoneId);
      adjusted = ZonedDateTime.ofInstant(cal.toInstant(), originalZoneId);
    } else if (original.isSupported(ChronoField.SECOND_OF_DAY)) {
      LocalDateTime originalAsTemporal = LocalDateTime.from(original);
      adjusted = originalAsTemporal.plusDays(offsetDays);
    } else {
      LocalDate originalAsTemporal = LocalDate.from(original);
      adjusted = originalAsTemporal.plusDays(offsetDays);
    }
    return adjusted;
  }
}
