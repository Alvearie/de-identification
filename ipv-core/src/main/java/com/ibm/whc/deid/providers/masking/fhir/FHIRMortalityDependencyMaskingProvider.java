/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import java.time.LocalDate;
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
    putField(i, null);

    // create or modify boolean field
    JsonNode newNode = isTooYoung(parent) ? NullNode.getInstance() : BooleanNode.getTrue();
    JsonNode boolNode = parent.get(BOOLEAN_FIELD);
    if (boolNode == null || boolNode.isNull() || boolNode.asText(null) == null) {
      parent.set(BOOLEAN_FIELD, newNode);
    } else if (boolNode.isValueNode()) { // expected
      parent.set(BOOLEAN_FIELD, newNode);
    } else { // boolean is a container node - not allowed
      StringBuilder buffer = new StringBuilder(80);
      buffer.append("Input data property ").append(BOOLEAN_FIELD)
          .append(" must be a value, not a JSON container object for masking rule ")
          .append(getName());
      logger.logError(LogCodes.WPH1013E, buffer.toString());
      throw new KeyedRuntimeException(LogCodes.WPH1028E, getName());
    }
  }

  protected void maskBoolean(MaskingActionInputIdentifier i, ObjectNode parent) {
    if (isTooYoung(parent)) {
      putField(i, null);
    }
  }

  protected boolean isTooYoung(ObjectNode parent) {
    boolean young = false;
    JsonNode birthNode = parent.get(BIRTHDATE_FIELD);
    if (birthNode == null || birthNode.isNull() || birthNode.asText(null) == null) {
      // birth date not provided - assume young
      young = true;
    } else if (birthNode.isValueNode()) { // expected
      String birthString = birthNode.asText();
      try {
        LocalDate birthDate = LocalDate.parse(birthString);
        LocalDate currentDate = LocalDate.now();
        long age = ChronoUnit.YEARS.between(currentDate, birthDate);
        young = age <= this.minYears;
      } catch (DateTimeParseException e) {
        logger.logWarn(LogCodes.WPH1012W, e, e.getMessage());
        StringBuilder buffer = new StringBuilder(80);
        buffer.append("Value in input for ").append(BIRTHDATE_FIELD)
            .append(" could not be parsed into a date - nullifying deceased indicator");
        logger.logError(LogCodes.WPH1013E, buffer.toString());
        young = true;
      }
    } else { // container node
      // invalid node type for birthdate - assume young
      young = true;
    }
    return young;
  }
}
