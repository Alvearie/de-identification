/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.util.List;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.BooleanNode;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.ibm.whc.deid.providers.masking.AbstractMaskingProvider;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.pojo.config.masking.FHIRMortalityDependencyMaskingProviderConfig;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

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
        throw new KeyedRuntimeException(LogCodes.WPH1028E, getName());
      }
      
      JsonNode parentRaw = i.getParent();
      if (!parentRaw.isObject()) {
        StringBuilder buffer = new StringBuilder(80);
        buffer.append("Parent node for property ").append(path).append(" for rule ").append(getName()).append(" must be a JSON object node");
        logger.logError(LogCodes.WPH1013E, buffer.toString());        
        throw new KeyedRuntimeException(LogCodes.WPH1028E, getName());
      }
      ObjectNode parent = (ObjectNode)parentRaw;

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
      // check for time component and strip it off if found (can be supplied in deceasedDateTime)
      int timeIndex = stringValue.indexOf('T');
      if (timeIndex > -1) {
        stringValue = stringValue.substring(0, timeIndex);
      }
      String[] components = stringValue.split("-");
      try {
        int year = Integer.parseInt(components[0]);
        int month = components.length > 1 ? Integer.parseInt(components[1]) : 1;
        int day = components.length > 2 ? Integer.parseInt(components[2]) : 1;
        date = LocalDate.of(year, month, day);
      } catch (NumberFormatException e) {
        StringBuilder buffer = new StringBuilder(80);
        // do not include the actual data in the log message
        buffer.append("Value in input for ").append(propertyName)
            .append(" could not be parsed into a date");
        logger.logError(LogCodes.WPH1013E, buffer.toString());
      }
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
