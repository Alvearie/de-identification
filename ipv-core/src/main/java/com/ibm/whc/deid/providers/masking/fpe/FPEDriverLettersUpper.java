/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import java.util.Locale;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;

public class FPEDriverLettersUpper extends FPEDriverBaseSinglePhase {

  @Override
  protected boolean isEncryptDigits() {
    return false;
  }

  @Override
  protected boolean isEncryptLower() {
    return false;
  }

  @Override
  protected boolean isEncryptUpper() {
    return true;
  }

  @Override
  protected Radix getRadix() {
    return Radix.LOWER;
  }

  @Override
  protected String processInput(String in, PositionManager posMgr, int padNeeded, Pad padding) {
    return shiftLettersToBase26(in.toLowerCase(Locale.US));
  }

  @Override
  protected String processOutput(String out, PositionManager posMgr, int padNeeded, Pad padding) {
    return shiftBase26ToLetters(out).toUpperCase(Locale.US);
  }
}
