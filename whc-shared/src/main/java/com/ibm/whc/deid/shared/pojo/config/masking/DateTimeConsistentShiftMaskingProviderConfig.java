/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import java.time.temporal.UnsupportedTemporalTypeException;
import java.util.List;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonValue;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

@JsonInclude(Include.NON_NULL)
public class DateTimeConsistentShiftMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -4316466219154105211L;

  /**
   * Determines where the shifted datetime value is ordered in relation to the original datetime
   * value.
   */
  public static enum DateShiftDirection {

    /**
     * The shifted value is before the original value.
     */
    BEFORE("before"),

    /**
     * The shifted value can be before or after the original value.
     */
    BEFORE_OR_AFTER("beforeOrAfter"),

    /**
     * The shifted value is after the original value.
     */
    AFTER("after");

    private String value;

    private DateShiftDirection(String val) {
      this.value = val;
    }

    @JsonValue
    public String getValue() {
      return value;
    }
  }

  public static final DateShiftDirection DEFAULT_DATE_SHIFT_DIRECTION =
      DateShiftDirection.BEFORE_OR_AFTER;

  private List<String> customFormats = null;
  private int dateShiftMinimumDays = 1;
  private int dateShiftMaximumDays = 365;
  private DateShiftDirection dateShiftDirection = DEFAULT_DATE_SHIFT_DIRECTION;
  private String patientIdentifierPath = "/patient/reference";
  private String salt = null;

  public DateTimeConsistentShiftMaskingProviderConfig() {
    type = MaskingProviderType.DATETIME_CONSISTENT_SHIFT;
  }

  public int getDateShiftMinimumDays() {
    return dateShiftMinimumDays;
  }

  public void setDateShiftMinimumDays(int dateShiftMinimumDays) {
    this.dateShiftMinimumDays = dateShiftMinimumDays;
  }

  public int getDateShiftMaximumDays() {
    return dateShiftMaximumDays;
  }

  public void setDateShiftMaximumDays(int dateShiftMaximumDays) {
    this.dateShiftMaximumDays = dateShiftMaximumDays;
  }

  public DateShiftDirection getDateShiftDirection() {
    return dateShiftDirection;
  }

  public void setDateShiftDirection(DateShiftDirection dateShiftDirection) {
    this.dateShiftDirection =
        dateShiftDirection == null ? DEFAULT_DATE_SHIFT_DIRECTION : dateShiftDirection;
  }

  public String getPatientIdentifierPath() {
    return patientIdentifierPath;
  }

  public void setPatientIdentifierPath(String patientIdentifierPath) {
    this.patientIdentifierPath = patientIdentifierPath;
  }

  public List<String> getCustomFormats() {
    return customFormats;
  }

  public void setCustomFormats(List<String> customFormats) {
    this.customFormats = customFormats;
  }

  public String getSalt() {
    return salt;
  }

  public void setSalt(String salt) {
    this.salt = salt;
  }

  @Override
  public void validate(DeidMaskingConfig maskingConfig)
      throws InvalidMaskingConfigurationException {
    super.validate(maskingConfig);
    if (dateShiftMinimumDays < 0) {
      throw new InvalidMaskingConfigurationException(
          "`dateShiftMinimumDays` must be greater than or equal to 0");
    }
    if (dateShiftMaximumDays < dateShiftMinimumDays) {
      throw new InvalidMaskingConfigurationException(
          "`dateShiftMaximumDays` must be greater than or equal to `dateShiftMinimumDays`");
    }
    if (patientIdentifierPath == null || patientIdentifierPath.trim().isEmpty()) {
      throw new InvalidMaskingConfigurationException("`patientIdentifierPath` is missing");
    }
    if (customFormats != null) {
      int offset = 0;
      for (String format : customFormats) {
        if (format == null || format.trim().isEmpty()) {
          throw new InvalidMaskingConfigurationException(
              "format at offset " + offset + " in `customFormats` is missing");
        }
        try {
          DateTimeMaskingProviderConfig.buildOverrideFormatter(format, null);
        } catch (IllegalArgumentException | UnsupportedTemporalTypeException e) {
          // thrown if the pattern is not a valid datetime pattern
          throw new InvalidMaskingConfigurationException(
              "format at offset " + offset + " in `customFormats` is not valid: " + e.getMessage(),
              e);
        }
        offset++;
      }
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + Objects.hash(customFormats, dateShiftDirection, dateShiftMaximumDays,
        dateShiftMinimumDays, patientIdentifierPath, salt);
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (!super.equals(obj)) {
      return false;
    }
    if (!(obj instanceof DateTimeConsistentShiftMaskingProviderConfig)) {
      return false;
    }
    DateTimeConsistentShiftMaskingProviderConfig other =
        (DateTimeConsistentShiftMaskingProviderConfig) obj;
    return Objects.equals(customFormats, other.customFormats)
        && dateShiftDirection == other.dateShiftDirection
        && dateShiftMaximumDays == other.dateShiftMaximumDays
        && dateShiftMinimumDays == other.dateShiftMinimumDays
        && Objects.equals(patientIdentifierPath, other.patientIdentifierPath)
        && Objects.equals(salt, other.salt);
  }
}
