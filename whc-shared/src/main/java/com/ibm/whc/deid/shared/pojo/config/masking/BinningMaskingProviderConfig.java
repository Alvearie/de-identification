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
 * Replaces a numerical input value with an interval that contains the value.
 */
@JsonInclude(Include.NON_NULL)
public class BinningMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -7017122633036050871L;

  int binSize = 5;
  String format = "%s-%s";
  int startValue = 0;
  boolean useStartValue = false;

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
    this.format = format;
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
}
