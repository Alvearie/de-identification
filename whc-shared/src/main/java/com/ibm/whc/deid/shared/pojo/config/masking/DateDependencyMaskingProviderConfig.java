/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;

/*
 * Provider for masking a field based on two related dates
 */
@JsonInclude(Include.NON_NULL)
public class DateDependencyMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 7784240554858910715L;

  String datetimeYearDeleteNIntervalMaskDate;
  String datetimeYearDeleteNIntervalCompareDate;
  String datetimeyearDeleteNIntervalCompareDateValue;

  int dateYearDeleteNDaysValue = 365;

  public String getDatetimeyearDeleteNIntervalCompareDateValue() {
    return datetimeyearDeleteNIntervalCompareDateValue;
  }

  public void setDatetimeyearDeleteNIntervalCompareDateValue(
      String datetimeyearDeleteNIntervalCompareDateValue) {
    this.datetimeyearDeleteNIntervalCompareDateValue = datetimeyearDeleteNIntervalCompareDateValue;
  }

  public String getDatetimeYearDeleteNIntervalMaskDate() {
    return datetimeYearDeleteNIntervalMaskDate;
  }

  public DateDependencyMaskingProviderConfig() {
    type = MaskingProviderType.DATEDEPENDENCY;
  }

  public void setDatetimeYearDeleteNIntervalMaskDate(String datetimeYearDeleteNIntervalMaskDate) {
    this.datetimeYearDeleteNIntervalMaskDate = datetimeYearDeleteNIntervalMaskDate;
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
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + dateYearDeleteNDaysValue;
    result = prime * result + ((datetimeYearDeleteNIntervalCompareDate == null) ? 0
        : datetimeYearDeleteNIntervalCompareDate.hashCode());
    result = prime * result + ((datetimeYearDeleteNIntervalMaskDate == null) ? 0
        : datetimeYearDeleteNIntervalMaskDate.hashCode());
    result = prime * result + ((datetimeyearDeleteNIntervalCompareDateValue == null) ? 0
        : datetimeyearDeleteNIntervalCompareDateValue.hashCode());
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
    if (datetimeYearDeleteNIntervalMaskDate == null) {
      if (other.datetimeYearDeleteNIntervalMaskDate != null)
        return false;
    } else if (!datetimeYearDeleteNIntervalMaskDate
        .equals(other.datetimeYearDeleteNIntervalMaskDate))
      return false;
    if (datetimeyearDeleteNIntervalCompareDateValue == null) {
      if (other.datetimeyearDeleteNIntervalCompareDateValue != null)
        return false;
    } else if (!datetimeyearDeleteNIntervalCompareDateValue
        .equals(other.datetimeyearDeleteNIntervalCompareDateValue))
      return false;
    return true;
  }
}
