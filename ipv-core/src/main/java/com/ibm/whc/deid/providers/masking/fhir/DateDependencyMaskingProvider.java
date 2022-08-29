/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalQueries;
import java.util.List;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.ibm.whc.deid.providers.identifiers.DateTimeIdentifier;
import com.ibm.whc.deid.providers.identifiers.DateTimeIdentifier.DateTimeParseResult;
import com.ibm.whc.deid.providers.masking.AbstractMaskingProvider;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateDependencyMaskingProviderConfig;
import com.ibm.whc.deid.util.RandomGenerators;

/**
 * This masking provider requires two date type fields in the same JSON object node, one for masking
 * and the other one to compare with the masked value. This compared value is attached to the
 * masking configuration and sent to date time masking provider. The comparison operation is done in
 * date time masking provider.
 */
public class DateDependencyMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 4938913531249878291L;

  private static final DateTimeIdentifier dateTimeIdentifier = new DateTimeIdentifier();

  private final String datetimeYearDeleteNIntervalCompareDate;
  private final int dateYearDeleteNDaysValue;

  public DateDependencyMaskingProvider(DateDependencyMaskingProviderConfig maskingConfiguration,
      DeidMaskingConfig deidMaskingConfig) {
    super(maskingConfiguration);
    this.datetimeYearDeleteNIntervalCompareDate =
        maskingConfiguration.getDatetimeYearDeleteNIntervalCompareDate();
    this.dateYearDeleteNDaysValue = maskingConfiguration.getDateYearDeleteNDaysValue();
  }

  @Override
  public void maskIdentifierBatch(List<MaskingActionInputIdentifier> identifiers) {
    for (MaskingActionInputIdentifier i : identifiers) {
      mask(i.getParent(), i.getPath());
    }
  }

  @Override
  public String mask(String identifier) {
    return null;
  }

  /**
   * This is the masking function for DateDependencyMaskingProvider.
   *
   * @param node the parent JSON object node which is expected to contain the property being masked
   *        and the property to which the current value of the property being masked is compared.
   * 
   * @param dateToMask the name of the property being masked
   * 
   * @return the updated node
   */
  protected JsonNode mask(JsonNode node, String dateToMask) {
    if (node != null && node.has(dateToMask) && node.has(datetimeYearDeleteNIntervalCompareDate)) {
      JsonNode maskNode = node.get(dateToMask);
      JsonNode compareNode = node.get(datetimeYearDeleteNIntervalCompareDate);
      if (!maskNode.isNull() && !compareNode.isNull()) {

        String maskDateValue = maskNode.asText();
        String compareDateValue = compareNode.asText();

        String maskedValue = doDateMasking(maskDateValue, compareDateValue);

        // Update the field with the masked value returned from
        // DateTimeMaskingProvider
        ((ObjectNode) node).put(dateToMask, maskedValue);
      }
    }

    return node;
  }

  protected String doDateMasking(String identifier, String targetDate) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    String maskedString;

    DateTimeParseResult parseResult = dateTimeIdentifier.parse(identifier);
    if (parseResult == null) {
      return applyUnexpectedValueHandling(identifier,
          () -> RandomGenerators.generateRandomDate(DateTimeFormatter.ISO_OFFSET_DATE_TIME));
    }
    TemporalAccessor d = parseResult.getValue();
    
    DateTimeParseResult compareParseResult = dateTimeIdentifier.parse(targetDate);
    if (compareParseResult == null) {
      return applyUnexpectedValueHandling(identifier,
          () -> RandomGenerators.generateRandomDate(DateTimeFormatter.ISO_OFFSET_DATE_TIME));
    }
    TemporalAccessor compareAccessor = compareParseResult.getValue();
    
    Instant datetimeInstant = null;
    LocalDateTime datetimeLocal = null;
    // temporals with a zoneId also have an offset - the offset is all we need
    ZoneOffset datetimeOffset = d.query(TemporalQueries.offset()); 
    if (datetimeOffset != null) {
      datetimeInstant = d.query(OffsetDateTime::from).toInstant();
    } else {
      datetimeLocal = d.query(LocalDateTime::from);
    }

    Instant compareInstant = null;
    LocalDateTime compareLocal = null;
    ZoneOffset compareOffset = compareAccessor.query(TemporalQueries.offset()); 
    if (compareOffset != null) {
      compareInstant = compareAccessor.query(OffsetDateTime::from).toInstant();
    } else {
      compareLocal = compareParseResult.getValue().query(LocalDateTime::from);
    }
    
    // if only one of the datetimes is an Instant (had a timezone offset), apply
    // that offset to the other datetime
    if (datetimeInstant == null && compareOffset != null) {
      datetimeInstant = datetimeLocal.atOffset(compareOffset).toInstant();
    } else if (compareInstant == null && datetimeOffset != null) {
      compareInstant = compareLocal.atOffset(datetimeOffset).toInstant();
    }
    
    // use Instants if available - if one is available, they both are
    if (datetimeInstant != null) {
      long daysBetween = Math.abs(ChronoUnit.DAYS.between(datetimeInstant, compareInstant));
      if (daysBetween <= dateYearDeleteNDaysValue) {
        maskedString = String.format("%02d/%02d", d.get(ChronoField.DAY_OF_MONTH),
            d.get(ChronoField.MONTH_OF_YEAR));
      } else {
        maskedString = identifier;
      }

    } else {
      long daysBetween = Math.abs(ChronoUnit.DAYS.between(datetimeLocal, compareLocal));
      if (daysBetween <= dateYearDeleteNDaysValue) {
        maskedString = String.format("%02d/%02d", d.get(ChronoField.DAY_OF_MONTH),
            d.get(ChronoField.MONTH_OF_YEAR));
      } else {
        maskedString = identifier;
      }
    }

    return maskedString;
  }
}
