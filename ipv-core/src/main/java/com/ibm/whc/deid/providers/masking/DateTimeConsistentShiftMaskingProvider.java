/*
 * © Merative US L.P. 2021,2022
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
import java.time.temporal.ChronoField;
import java.time.temporal.Temporal;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalQueries;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;
import com.fasterxml.jackson.databind.JsonNode;
import com.ibm.whc.deid.jsonpath.JSONPath;
import com.ibm.whc.deid.providers.identifiers.DateTimeIdentifier;
import com.ibm.whc.deid.providers.identifiers.DateTimeIdentifier.DateTimeParseResult;
import com.ibm.whc.deid.providers.masking.fhir.MaskingActionInputIdentifier;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeConsistentShiftMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeConsistentShiftMaskingProviderConfig.DateShiftDirection;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeMaskingProviderConfig;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import com.ibm.whc.deid.util.HashUtils;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import com.ibm.whc.deid.utils.log.Messages;

public class DateTimeConsistentShiftMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -1754981791730429377L;

  protected static final LogManager logger = LogManager.getInstance();

  private static final DateTimeIdentifier dateTimeIdentifier = new DateTimeIdentifier();

  private final int dateShiftMinimumDays;
  private final int dateShiftMaximumDays;
  private final DateShiftDirection dateShiftDirection;
  private final String salt;
  private final List<String> customFormats;
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
    this.salt = configuration.getSalt();
    this.customFormats = configuration.getCustomFormats();

    // path must start with '/' as required for JsonPointer
    String path = configuration.getPatientIdentifierPath().trim();
    if (path.charAt(0) != '/') {
      path = "/" + path;
    }
    this.compiledPathExp = JSONPath.compile(path);
  }

  @Override
  public String mask(String identifier) {
    throw new IllegalStateException("should not be reached");
  }

  @Override
  public void maskIdentifierBatch(List<MaskingActionInputIdentifier> identifiers) {
    // DateTimeFormatter is not Serializable, so build instances as method variables
    List<DateTimeFormatter> customFormatters = buildCustomFormatters();

    for (MaskingActionInputIdentifier maskingActionInputIdentifier : identifiers) {
      String replacementValue = generateReplacement(maskingActionInputIdentifier, customFormatters);
      // set the replacement value into the record
      putField(maskingActionInputIdentifier, replacementValue);
    }
  }
  
  /**
   * Builds date time formatting objects from the configured custom format patterns, if any.
   * 
   * @return a possibly-null, non-empty list of custom date time formatters
   */
  protected List<DateTimeFormatter> buildCustomFormatters() {
    List<DateTimeFormatter> customFormatters = null;
    if (this.customFormats != null && !this.customFormats.isEmpty()) {
      customFormatters = new ArrayList<>(this.customFormats.size());
      for (String format : this.customFormats) {        
        customFormatters.add(DateTimeMaskingProviderConfig.buildOverrideFormatter(format, null));
      }
    }
    return customFormatters;
  }

  /**
   * Perform privacy protection operation on the given masking input
   * 
   * @param maskingActionInputIdentifier the masking input
   * @param customFormatters the customer-provided formatters - used before builtin formatters
   * 
   * @return the value resulting from applying the privacy protection operation
   */
  protected String generateReplacement(MaskingActionInputIdentifier maskingActionInputIdentifier,
      List<DateTimeFormatter> customFormatters) {
    String originalValue = maskingActionInputIdentifier.getNode().asText();
    if (originalValue == null || originalValue.trim().isEmpty()) {
      return applyUnexpectedValueHandling(String.valueOf(originalValue), null);
    }

    String patientId = getPatientIdentifier(maskingActionInputIdentifier);
    if (patientId == null || patientId.trim().isEmpty()) {
      return applyUnexpectedValueHandling("patient identifier `" + patientId + "`", null);
    }

    int offsetDays = generateShiftNumberOfDays(patientId);

    return applyOffsetAndReformat(originalValue, offsetDays, customFormatters);
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
    if (this.dateShiftDirection == DateShiftDirection.BEFORE_OR_AFTER) {
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
      case BEFORE:
        shfitNumberOfDays = -this.dateShiftMaximumDays + possiblesIndex;
        break;
      case AFTER:
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
   * Generate a non-negative, consistent long value from a given string. Each time the same string
   * is presented, the same long is returned.
   * 
   * <p>
   * Note that there is no guarantee that the same long is not returned for various different
   * strings, only that the same string will return the same long. In particular leading and
   * trailing whitespace and differences in character case do not alter the long values generated
   * for different strings. This is a convenience so that minor alterations in the input data do not
   * adversely impact results. For example, the same long is generated for the input values
   * "patient1", "patient1 ", "Patient1".
   * 
   * @param seed the string from which a long numeric value is to be generated
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

    long generatedLong = HashUtils.longFromHash(seed) * 31L;
    
    // if the salt parameter is not null or all whitespace, use it modify the generated long
    if (this.salt != null && !this.salt.trim().isEmpty()) {
      long saltLong = HashUtils.longFromHash(this.salt);
      generatedLong += saltLong;
    }

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
   * @param customFormatters the customer-provided formatters - used before builtin formatters
   * 
   * @return the shifted temporal value in the same string format as the given value
   */
  protected String applyOffsetAndReformat(String originalValue, int offsetDays, List<DateTimeFormatter> customFormatters) {

    DateTimeParseResult parseResponse = parse(originalValue, customFormatters);
    if (parseResponse == null) {
      return applyUnexpectedValueHandling(originalValue, null);
    }
    DateTimeFormatter formatter = parseResponse.getFormatter();
    TemporalAccessor parsedOriginal = parseResponse.getValue();

    Temporal adjustedTemporal = adjust(parsedOriginal, offsetDays);
    if (adjustedTemporal == null) {
      // Should only happen for custom formatters.
      // The incoming data has been parsed according to one of the formats, but
      // it was not possible to obtain a LocalDateTime from it. Since a time
      // component is defaulted when all the formatters are built, the most
      // likely cause is a custom format did not provide enough information
      // to obtain a year, month, and date. This should be considered an
      // invalid configuration and processing should not have started. However,
      // there is no feasible way to test a custom input pattern without trying
      // real input data to catch these errors earlier. The best alternative
      // is to stop processing now.
      String msg = Messages.getMessage(LogCodes.WPH1025W, parseResponse.getPattern());
      StringBuilder buffer = new StringBuilder(msg.length() + LogCodes.WPH1025W.length() + 1);
      buffer.append(LogCodes.WPH1025W).append(' ').append(msg);
      throw new IllegalArgumentException(buffer.toString());
    }

    String replacement = formatter.format(adjustedTemporal);

    if (parseResponse.isVariableCase()) {
      if (originalValue.equals(originalValue.toUpperCase())) {
        replacement = replacement.toUpperCase();
      } else if (originalValue.equals(originalValue.toLowerCase())) {
        replacement = replacement.toLowerCase();
      }
    }
    return replacement;
  }

  /**
   * Parse the given date or date and time string into a temporal object and return that object
   * along with the formatter that recognized and converted it.
   * 
   * @param originalValue a date or date and time value in string form
   * @param customFormatters the customer-provided formatters - used before builtin formatters
   * 
   * @return a simple container holding the formatter that recognized the form of the string value,
   *         the pattern string (null if a builtin formatter recognized the value), and the temporal
   *         object created by parsing the string
   */
  protected DateTimeParseResult parse(String originalValue,
      List<DateTimeFormatter> customFormatters) {
    DateTimeParseResult parseResponse = null;

    if (customFormatters != null) {
      int index = 0;
      for (DateTimeFormatter formatter : customFormatters) {
        try {
          TemporalAccessor accessor = formatter.parse(originalValue);
          parseResponse =
              new DateTimeParseResult(formatter, customFormats.get(index), accessor, false);
          break;
        } catch (Exception e) {
          StringBuilder buffer = new StringBuilder(120);
          buffer.append("could not parse with custom format `")
              .append(customFormats.get(index)).append("`: ").append(e.getMessage())
              .append(" - trying other formats");
          log.logInfo(LogCodes.WPH1000I, buffer.toString());
          // continue processing
        }
        index++;
      }
    }

    if (parseResponse == null) {
      parseResponse = dateTimeIdentifier.parse(originalValue);
    }

    return parseResponse;
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
   * @return the adjusted temporal object or <i>null</i> if the original object does not have
   *         sufficient information to provide a date that can be adjusted
   */
  protected Temporal adjust(TemporalAccessor original, int offsetDays) {
    Temporal adjusted = null;
    ZoneId originalZoneId = original.query(TemporalQueries.zone());
    if (originalZoneId != null && original.isSupported(ChronoField.INSTANT_SECONDS)) {
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
    } else if (original.isSupported(ChronoField.EPOCH_DAY)
        && original.isSupported(ChronoField.NANO_OF_DAY)) {
      // No time zone offset information is available, but a time portion is provided
      LocalDateTime originalAsTemporal = LocalDateTime.from(original);
      adjusted = originalAsTemporal.plusDays(offsetDays);
    } else {
      LocalDate originalAsTemporal = original.query(TemporalQueries.localDate());
      if (originalAsTemporal != null) {
        adjusted = originalAsTemporal.plusDays(offsetDays);
      }
    }
    return adjusted;
  }
}
