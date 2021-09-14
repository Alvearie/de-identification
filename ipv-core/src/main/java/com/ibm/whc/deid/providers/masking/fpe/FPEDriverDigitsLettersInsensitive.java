/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import java.util.Locale;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;

public class FPEDriverDigitsLettersInsensitive extends FPEDriverBaseSinglePhase {

  private OutputCase outputCase;

  public FPEDriverDigitsLettersInsensitive(OutputCase outputCase) {
    this.outputCase = outputCase;
  }

  @Override
  protected boolean isEncryptDigits() {
    return true;
  }

  @Override
  protected boolean isEncryptLower() {
    return true;
  }

  @Override
  protected boolean isEncryptUpper() {
    return true;
  }

  @Override
  protected Radix getRadix() {
    return Radix.DIGITS_LOWER;
  }

  @Override
  protected String processInput(String in, PositionManager posMgr, int padNeeded, Pad padding) {
    return in.toLowerCase(Locale.US);
  }

  @Override
  protected String processOutput(String out, PositionManager posMgr, int padNeeded, Pad padding) {
    // only UPPER and LOWER are expected, only UPPER requires any action
    return outputCase == OutputCase.UPPER ? out.toUpperCase(Locale.US) : out;
  }
}
