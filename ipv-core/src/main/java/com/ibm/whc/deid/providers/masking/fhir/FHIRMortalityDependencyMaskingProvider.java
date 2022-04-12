/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.List;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.BooleanNode;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.ibm.whc.deid.providers.masking.AbstractMaskingProvider;
import com.ibm.whc.deid.shared.pojo.config.masking.FHIRMortalityDependencyMaskingProviderConfig;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import com.ibm.whc.deid.utils.log.Messages;

public class FHIRMortalityDependencyMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -3890925036519369771L;

  private static final LogManager logger = LogManager.getInstance();

  public static final String DATE_TIME_FIELD = "deceasedDateTime";
  public static final String BOOLEAN_FIELD = "deceasedBoolean";
  public static final String BIRTHDATE_FIELD = "birthDate";

  private final int minYears;

  public FHIRMortalityDependencyMaskingProvider(
      FHIRMortalityDependencyMaskingProviderConfig maskingConfiguration) {
    this.minYears = maskingConfiguration.getMortalityIndicatorMinYears();
  }

  @Override
  public void maskIdentifierBatch(List<MaskingActionInputIdentifier> identifiers) {
    for (MaskingActionInputIdentifier i : identifiers) {
      
      String path = i.getPath();
      if (!DATE_TIME_FIELD.equals(path) && !BOOLEAN_FIELD.equals(path)) {
        StringBuilder buffer = new StringBuilder(80);
        buffer.append("Target property for rule ").append(getName()).append(" must be ").append(DATE_TIME_FIELD).append(" or ").append(BOOLEAN_FIELD).append(" but was ").append(path);
        logger.logError(LogCodes.WPH1013E, buffer.toString());        
        throw new IllegalArgumentException(Messages.getMessage(LogCodes.WPH1028E, path, getName()));
      }
      
      // Parent should always be ObjectNode by this point.
      // ArrayNode parents would generate path values like deceasedBoolean[0] that wouldn't match
      // the allowed paths.
      ObjectNode parent = (ObjectNode) i.getParent();
      if (DATE_TIME_FIELD.equals(path)) {
        maskDateTime(i, parent);
      } else {
        maskBoolean(i, parent);
      }
    }
  }

  @Override
  public String mask(String identifier) {
    // should not be called
    return null;
  }

  protected void maskDateTime(MaskingActionInputIdentifier i, ObjectNode parent) {
    // The datetime value of this node will always be removed.
    // Determine whether the related boolean property should be set to True or
    // removed completely, based on the age of the person when mortality occurred.
    // This must be checked before the value of this node is removed.
    JsonNode newNode =
        isBooleanRemovalNeeded(parent) ? NullNode.getInstance() : BooleanNode.getTrue();
    parent.set(BOOLEAN_FIELD, newNode);

    putField(i, null);
  }

  protected void maskBoolean(MaskingActionInputIdentifier i, ObjectNode parent) {
    // get the value of this node
    boolean deceased = true;
    JsonNode node = parent.get(BOOLEAN_FIELD);
    // being conservative, so unless the value is explicitly false, consider the person deceased
    if (node != null && !node.isNull() && node.isValueNode()
        && "false".equalsIgnoreCase(node.asText())) {
      deceased = false;
    }
    // if deceased, remove value completely based on patient age
    if (deceased && isBooleanRemovalNeeded(parent)) {
      putField(i, null);
    }
  }

  protected LocalDate getDateFromNode(ObjectNode parent, String propertyName) {
    LocalDate date = null;
    JsonNode node = parent.get(propertyName);
    if (node != null && !node.isNull() && node.isValueNode() && node.asText(null) != null) {
      String stringValue = node.asText();
      date = getDateFromString(stringValue, propertyName);
    }
    return date;
  }

  protected LocalDate getDateFromString(String inStringValue, String propertyName) {
    LocalDate date = null;
    // check for time component and strip it off if found (can be supplied in deceasedDateTime)
    int timeIndex = inStringValue.indexOf('T');
    String stringValue = timeIndex > -1 ? inStringValue.substring(0, timeIndex) : inStringValue;
    try {
      String[] components = stringValue.split("-");
      if (components.length < 3) {
        // get the year and month, if provided, and create LocalDate from those
        if (components[0].length() != 4) {
          throw new RuntimeException("invalid year");
        }
        int year = Integer.parseInt(components[0]);
        if (year < 0 || year > 9999) {
          throw new RuntimeException("invalid year");
        }
        int month;
        if (components.length == 2) {
          if (components[1].length() != 2) {
            throw new RuntimeException("invalid month");
          }
          month = Integer.parseInt(components[1]);
          if (month < 1 || month > 12) {
            throw new RuntimeException("invalid month)");
          }
        } else {
          month = 1;
        }
        date = LocalDate.of(year, month, 1);
      } else {
        // there is enough data provided for LocaleDate to parse the value
        date = LocalDate.parse(stringValue);
      }
    } catch (Exception e) {
      StringBuilder buffer = new StringBuilder(80);
      // do not include the actual data in the log message
      buffer.append("Value in input for ").append(propertyName)
          .append(" could not be parsed into a date");
      logger.logError(LogCodes.WPH1013E, buffer.toString());
    }
    return date;
  }

  protected boolean isBooleanRemovalNeeded(ObjectNode parent) {
    boolean remove = false;
    LocalDate birthDate = getDateFromNode(parent, BIRTHDATE_FIELD);
    if (birthDate == null) {
      // not sure the age of the person, therefore assume deceased indicators must be removed
      remove = true;
    } else {
      // get the deceased date value as a date
      LocalDate mortalityDate = getDateFromNode(parent, DATE_TIME_FIELD);
      if (mortalityDate == null) {
        // not sure when the person became deceased, compare the birthdate to the current date
        LocalDate currentDate = LocalDate.now();
        remove = isTooYoung(birthDate, currentDate);
      } else {
        remove = isTooYoung(birthDate, mortalityDate);
      }
    }
    return remove;
  }

  protected boolean isTooYoung(LocalDate birthDate, LocalDate compareDate) {
    long age = ChronoUnit.YEARS.between(birthDate, compareDate);
    boolean young = age <= this.minYears;
    return young;
  }
}
