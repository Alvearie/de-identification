/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.pojo.config.masking.BinningMaskingProviderConfig;

public class BinningMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = 8750115822634982432L;

  private final int binSize;
  private final String format;
  private final int startValue;
  private final boolean useStartValue;

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
  }

  @Override
  public String mask(String identifier) {
    int lowerBase = 0;
    if (useStartValue) {
      lowerBase = startValue;
    } else {
      double value = Double.valueOf(identifier);

      int intValue = (int) value;

      lowerBase = intValue - (intValue % binSize);
    }
    int higherBase = lowerBase + binSize;

    return String.format(this.format, lowerBase, higherBase);
  }
}
