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
    String output;

    PositionManager posMgr = new PositionManager(in);

    String input = posMgr.extract(isEncryptDigits(), isEncryptLower(), isEncryptUpper());

    if (input.isEmpty()) {
      // nothing to encrypt, just return original value
      output = in;

    } else {
      int padNeeded = calculatePadNeeded(input, getRadix());
      if (padNeeded > 0) {
        input = addPadding(input, padNeeded, padding, getRadix());
      }

      input = processInput(input, posMgr, padNeeded, padding);

      String encrypted = encrypt(input, key, tweak, getRadix());

      encrypted = processOutput(encrypted, posMgr, padNeeded, padding);

      output = replaceSymbols(padNeeded, padding, posMgr, encrypted, isEncryptDigits(),
          isEncryptLower(), isEncryptUpper());
    }

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
