/*
 * © Merative US L.P. 2016,2021
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
  private final int normalizedStartValue;
  private final boolean useSingleBucketOverThreshold;
  private final double singleBucketOverThresholdValue;
  private final String singleBucketOverThresholdReplacement;
  private final boolean useSingleBucketUnderThreshold;
  private final double singleBucketUnderThresholdValue;
  private final String singleBucketUnderThresholdReplacement;

  /**
   * Instantiates a new Binning masking provider.
   *
   * @param config a BinningMaskingProviderConfig instance
   */
  public BinningMaskingProvider(BinningMaskingProviderConfig config) {
    super(config);
    this.binSize = config.getBinSize();
    this.format = config.getFormat();
    this.startValue = config.getStartValue();
    this.useStartValue = config.isUseStartValue();
    this.normalizedStartValue = normalizeStartValue();
    this.singleBucketOverThresholdReplacement = config.getSingleBucketOverThresholdReplacement();
    this.singleBucketOverThresholdValue = config.getSingleBucketOverThresholdValue();
    this.singleBucketUnderThresholdReplacement = config.getSingleBucketUnderThresholdReplacement();
    this.singleBucketUnderThresholdValue = config.getSingleBucketUnderThresholdValue();
    this.useSingleBucketOverThreshold = config.isUseSingleBucketOverThreshold();
    this.useSingleBucketUnderThreshold = config.isUseSingleBucketUnderThreshold();

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
      return applyUnexpectedValueHandling(identifier, null);
    }

    if (this.useSingleBucketOverThreshold == true) {

      if (value >= this.singleBucketOverThresholdValue) {
        return this.singleBucketOverThresholdReplacement;
      }
    }

    if (this.useSingleBucketUnderThreshold == true) {

      if (value < this.singleBucketUnderThresholdValue) {
        return this.singleBucketUnderThresholdReplacement;
      }
    }

    double adjusted = value - this.normalizedStartValue;
    double binCount = adjusted / this.binSize;
    // if negative, move down to lower integer
    long binsToApply = (long) Math.floor(binCount);

    long lowerBase = normalizedStartValue + (this.binSize * binsToApply);
    long higherBase = lowerBase + this.binSize;

    return String.format(this.format, Long.valueOf(lowerBase), Long.valueOf(higherBase));
  }

}
