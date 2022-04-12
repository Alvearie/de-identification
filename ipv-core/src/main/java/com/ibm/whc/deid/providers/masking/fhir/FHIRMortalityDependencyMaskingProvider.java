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

/**
 * Privacy provider that enforces a specific type of dependency between the birth date and the
 * deceased datetime and deceased boolean properties that exist in a FHIR Patient resource, which is
 * a very specific schema imposed on a JSON document.
 * 
 * <p>
 * This provider may only be applied to properties named either exactly "deceasedDateTime" or
 * "deceasedBoolean". In is recommended to configure the provider on both of these properties. It is
 * expected that both of these properties and another property named "birthDate" are, if included in
 * the document, all belong to the same parent node in the JSON document.
 * 
 * <p>
 * When the privacy provider is applied to "deceasedDateTime" and "deceasedDateTime" has a
 * <i>non-null</i> value in the input document, the value of "deceasedDateTime" is set to
 * <i>null</i> In addition, the value of "deceasedBoolean" is either set to <i>true</i> or
 * <i>null</i> depending upon the age of this patient when death occurred. If "deceasedBoolean" does
 * not exist as a sibling of "deceasedDateTime", it will be added to the JSON structure.
 * 
 * <p>
 * The age of the patient at the time of death is determined by comparing the value of the
 * "deceasedDateTime" property with the value of the "birthDate" property. If the value of either
 * property contains a 'T', which indicates the beginning of a time component in a DateTime value in
 * FHIR, the portion of the value beginning with the 'T' is dropped. The remainder of the value is
 * matched to one of these allowed formats:
 * <ul>
 * <li>yyyy = four digit year
 * <li>yyyy-MM = four digit year, two digit month
 * <li>yyyy-MM-dd = four digit year, two digit month, two digit day
 * </ul>
 * The formats are not lenient. All components of the value must be provided at their full length,
 * no other separator besides the dash (-) is allowed, and the resulting date value must be a valid
 * calendar date.
 * 
 * <p>
 * If the number of complete years between "birthDate" and "deceasedDateTime" is less than or equal
 * to the value of the privacy provider's configuration property "mortalityIndicatorMinYears", the
 * "deceasedBoolean" field is set to <i>null</i>. If the number of completed years is greater than
 * the configuration property value, "deceasedBoolean" is set to <i>true</i>.
 * 
 * <p>
 * Note, however, that when the configured value of "mortalityIndicatorMinYears" is less than 0, the
 * patient is always old enough to allow the "deceasedBoolean" indicator to remain in the data. In
 * such cases the date values are not retrieved and checked.
 * 
 * <p>
 * If the "birthDate" property does not exist in the same JSON object as "deceasedDateTime" or if it
 * has a <i>null</i> value or if its value cannot be parsed, "deceasedBoolean" is set to
 * <i>null</i>.
 * 
 * <p>
 * If the value of "deceasedDateTime" cannot be parsed, the current date is used as the date of
 * death and is compared to "birthDate" as described above.
 * 
 * <p>
 * When the privacy provider is applied to "deceasedBoolean" and "deceasedBoolean" has any value
 * other than "false" (case-insensitive if provided as a string), the "birthDate" value is compared
 * to the current date as described above. If the number of completed years between those dates is
 * less than or equal to the configuration property value or if the "birthDate" value cannot be
 * found or processed as described above, "deceasedBoolean" is set to <i>null</i>.
 */
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
  public String mask(String identifier) {
    // this method should not be called since maskIdentifierBatch() is overridden to
    // handles all masking itself
    return null;
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

  protected void maskDateTime(MaskingActionInputIdentifier i, ObjectNode parent) {
    // The datetime value of this node will always be removed.
    // Determine whether the related boolean property should be set to True or
    // removed completely, based on the age of the person when mortality occurred.
    // This must be checked before the value of this node is removed.
    JsonNode newNode =
        isBooleanRemovalNeeded(parent) ? NullNode.getInstance() : BooleanNode.getTrue();
    parent.set(BOOLEAN_FIELD, newNode);

    // set the value of this node to null
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
    // if deceased, remove value completely based on maximum possible age the patient might have
    // reached
    if (deceased && isBooleanRemovalNeeded(parent)) {
      putField(i, null);
    }
  }

  protected boolean isBooleanRemovalNeeded(ObjectNode parent) {
    boolean remove = false;
    // If the removal threshold is less than 0, then the patient is always
    // old enough to allow the boolean value to remain in the data - do not
    // even retrieve the birth date in that case.
    if (this.minYears >= 0) {
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
    }
    return remove;
  }

  protected boolean isTooYoung(LocalDate birthDate, LocalDate compareDate) {
    long age = ChronoUnit.YEARS.between(birthDate, compareDate);
    boolean young = age <= this.minYears;
    return young;
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
    // check for time component and strip it off if found (FHIR would allow it in deceasedDateTime)
    int timeIndex = inStringValue.indexOf('T');
    String stringValue = timeIndex > -1 ? inStringValue.substring(0, timeIndex) : inStringValue;
    try {
      String[] components = stringValue.split("-");
      if (components.length < 3) {
        // LocalDate.parse() doesn't handle dates that are just year or year/month
        // LocalDate.parse() requires four digit year, so enforce here for consistency
        if (components[0].length() != 4) {
          throw new RuntimeException("invalid year");
        }
        int year = Integer.parseInt(components[0]);
        if (year < 0 || year > 9999) {
          throw new RuntimeException("invalid year");
        }
        int month;
        if (components.length > 1) {
          // LocalDate.parse() requires two digit month, so enforce here for consistency
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
      // allow null to be returned
    }
    return date;
  }
}
