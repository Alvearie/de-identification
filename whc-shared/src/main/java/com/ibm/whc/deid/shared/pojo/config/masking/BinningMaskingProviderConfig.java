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
 * Replaces a numerical input value with an interval that contains the value.
 */
@JsonInclude(Include.NON_NULL)
public class BinningMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -7017122633036050871L;

  protected static final String DEFAULT_FORMAT = "%s-%s";

  private int binSize = 5;
  private String format = DEFAULT_FORMAT;
  private int startValue = 0;
  private boolean useStartValue = false;

  public BinningMaskingProviderConfig() {
    type = MaskingProviderType.BINNING;
  }

  public int getBinSize() {
    return binSize;
  }

  public void setBinSize(int binSize) {
    this.binSize = binSize;
  }

  public String getFormat() {
    return format;
  }

  public void setFormat(String format) {
    this.format = format == null ? DEFAULT_FORMAT : format;
  }

  public int getStartValue() {
    return startValue;
  }

  public void setStartValue(int startValue) {
    this.startValue = startValue;
  }

  public boolean isUseStartValue() {
    return useStartValue;
  }

  public void setUseStartValue(boolean useStartValue) {
    this.useStartValue = useStartValue;
  }

  @Override
  public void validate() throws InvalidMaskingConfigurationException {
    if (binSize < 1) {
      throw new InvalidMaskingConfigurationException("`binSize` must be greater than 0");
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + binSize;
    result = prime * result + (format == null ? 0 : format.hashCode());
    result = prime * result + startValue;
    result = prime * result + (useStartValue ? 1231 : 1237);
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (!super.equals(obj)) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    BinningMaskingProviderConfig other = (BinningMaskingProviderConfig) obj;
    if (binSize != other.binSize) {
      return false;
    }
    if (format == null ? other.format != null : !format.equals(other.format)) {
      return false;
    }
    if (startValue != other.startValue) {
      return false;
    }
    if (useStartValue != other.useStartValue) {
      return false;
    }
    return true;
  }

}
