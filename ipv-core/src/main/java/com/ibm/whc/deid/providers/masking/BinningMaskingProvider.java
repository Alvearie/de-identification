/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.pojo.config.masking.BinningMaskingProviderConfig;

/**
 * Privacy provider that replaces numeric values with a range that contains the original value.
 */
public class BinningMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 8750115822634982432L;

  private final int binSize;
  private final String format;
  private final int startValue;
  private final boolean useStartValue;
  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;
  private final int normalizedStartValue;

  /**
   * Instantiates a new Binning masking provider.
   *
   * @param config a BinningMaskingProviderConfig instance
   */
  public BinningMaskingProvider(BinningMaskingProviderConfig config) {
    this.binSize = config.getBinSize();
    this.format = config.getFormat();
    this.startValue = config.getStartValue();
    this.useStartValue = config.isUseStartValue();
    this.unspecifiedValueHandling = config.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = config.getUnspecifiedValueReturnMessage();
    this.normalizedStartValue = normalizeStartValue();
  }

  // normalize start value to lowest positive bin start value
  private int normalizeStartValue() {
    int nStartValue = 0;
    if (this.useStartValue) {
      int count = this.startValue / this.binSize;
      // count will be negative if startValue is negative
      nStartValue = this.startValue - (count * this.binSize);
      if (nStartValue < 0) {
        // advance to positive value
        nStartValue += this.binSize;
      }
    }
    return nStartValue;
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    double value;
    try {
      value = Double.parseDouble(identifier);
    } catch (NumberFormatException e) {
      // For this provider, we do not return a random value
      return unspecifiedValueHandling == 3 ? unspecifiedValueReturnMessage : null;
    }

    double adjusted = value - this.normalizedStartValue;
    double binCount = adjusted / this.binSize;
    // if negative, move down to lower integer
    long binsToApply = (long) Math.floor(binCount);

    long lowerBase = normalizedStartValue + (this.binSize * binsToApply);
    long higherBase = lowerBase + this.binSize;

    return String.format(this.format, new Long(lowerBase), new Long(higherBase));
  }
}
