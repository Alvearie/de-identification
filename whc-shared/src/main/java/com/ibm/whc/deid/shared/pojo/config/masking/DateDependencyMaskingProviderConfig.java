/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/*
 * Provider for masking a field based on two related dates in the same JSON object.
 */
@JsonInclude(Include.NON_NULL)
public class DateDependencyMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 7784240554858910715L;

  /**
   * The name of the property that is being compared with the value of the property being masked.
   * This property must exist within the same parent JSON object node as the property being masked.
   */
  private String datetimeYearDeleteNIntervalCompareDate;

  /**
   * The maximum number of days that between the target property value and the compared property
   * value for the masking action to occur.
   */
  private int dateYearDeleteNDaysValue = 365;

  public DateDependencyMaskingProviderConfig() {
    type = MaskingProviderType.DATEDEPENDENCY;
  }

  public String getDatetimeYearDeleteNIntervalCompareDate() {
    return datetimeYearDeleteNIntervalCompareDate;
  }

  public void setDatetimeYearDeleteNIntervalCompareDate(
      String datetimeYearDeleteNIntervalCompareDate) {
    this.datetimeYearDeleteNIntervalCompareDate = datetimeYearDeleteNIntervalCompareDate;
  }

  public int getDateYearDeleteNDaysValue() {
    return dateYearDeleteNDaysValue;
  }

  public void setDateYearDeleteNDaysValue(int dateYearDeleteNDaysValue) {
    this.dateYearDeleteNDaysValue = dateYearDeleteNDaysValue;
  }

  @Override
  public void validate() throws InvalidMaskingConfigurationException {
    super.validate();
    if (datetimeYearDeleteNIntervalCompareDate == null
        || datetimeYearDeleteNIntervalCompareDate.trim().isEmpty()) {
      throw new InvalidMaskingConfigurationException(
          "`datetimeYearDeleteNIntervalCompareDate` is required");
    }
    if (dateYearDeleteNDaysValue < 0) {
      throw new InvalidMaskingConfigurationException(
          "`dateYearDeleteNDaysValue` must be greater than or equal to 0");
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + dateYearDeleteNDaysValue;
    result = prime * result + ((datetimeYearDeleteNIntervalCompareDate == null) ? 0
        : datetimeYearDeleteNIntervalCompareDate.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (!super.equals(obj))
      return false;
    if (getClass() != obj.getClass())
      return false;
    DateDependencyMaskingProviderConfig other = (DateDependencyMaskingProviderConfig) obj;
    if (dateYearDeleteNDaysValue != other.dateYearDeleteNDaysValue)
      return false;
    if (datetimeYearDeleteNIntervalCompareDate == null) {
      if (other.datetimeYearDeleteNIntervalCompareDate != null)
        return false;
    } else if (!datetimeYearDeleteNIntervalCompareDate
        .equals(other.datetimeYearDeleteNIntervalCompareDate))
      return false;
    return true;
  }
}
