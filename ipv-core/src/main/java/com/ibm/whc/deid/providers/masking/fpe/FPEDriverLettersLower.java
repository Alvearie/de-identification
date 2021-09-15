/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;

/**
 * Concrete subclass for processing sequences of lower case letters.
 */
public class FPEDriverLettersLower extends FPEDriverBaseSinglePhase {

  @Override
  protected boolean isEncryptDigits() {
    return false;
  }

  @Override
  protected boolean isEncryptLower() {
    return true;
  }

  @Override
  protected boolean isEncryptUpper() {
    return false;
  }

  @Override
  protected Radix getRadix() {
    return Radix.LOWER;
  }

  @Override
  protected String processInput(String in, PositionManager posMgr, int padNeeded, Pad padding) {
    return shiftLettersToBase26(in);
  }

  @Override
  protected String processOutput(String out, PositionManager posMgr, int padNeeded, Pad padding) {
    return shiftBase26ToLetters(out);
  }
}
