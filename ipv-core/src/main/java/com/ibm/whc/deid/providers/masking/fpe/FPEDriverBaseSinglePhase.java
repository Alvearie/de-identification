/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;

public abstract class FPEDriverBaseSinglePhase extends FPEDriverBase {

  @Override
  public String encrypt(String in, String key, String tweak, Pad padding)
      throws UnsupportedLengthException, EncryptionEngineException {

    PositionManager posMgr = new PositionManager(in);

    String input = posMgr.extract(isEncryptDigits(), isEncryptLower(), isEncryptUpper());

    int padNeeded = calculatePadNeeded(input, getRadix());
    if (padNeeded > 0) {
      input = addPadding(input, padNeeded, padding, getRadix());
    }

    input = processInput(input, posMgr, padNeeded, padding);

    String encrypted = encrypt(input, key, tweak, getRadix());

    encrypted = processOutput(encrypted, posMgr, padNeeded, padding);

    // adjust offsets if padding was added to the front
    // no adjustment needed if padding was added to the back
    int padAdjust = padNeeded > 0 && padding == Pad.FRONT ? padNeeded : 0;
    String output = replaceSymbols(posMgr, encrypted, padAdjust, isEncryptDigits(),
        isEncryptLower(), isEncryptUpper());

    return output;
  }

  protected abstract boolean isEncryptDigits();

  protected abstract boolean isEncryptLower();

  protected abstract boolean isEncryptUpper();

  protected abstract Radix getRadix();

  protected String processInput(String in, PositionManager posMgr, int padNeeded, Pad padding) {
    // nothing required - use input as-is
    return in;
  }

  protected String processOutput(String out, PositionManager posMgr, int padNeeded, Pad padding) {
    // nothing required - use output as-is
    return out;
  }
}
