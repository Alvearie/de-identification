/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;

public class FPEDriverDigits extends FPEDriverBase {

  private static final Radix RADIX = Radix.DIGITS;

  @Override
  public String encrypt(String in, String key, String tweak, Pad padding)
      throws UnsupportedLengthException, EncryptionEngineException {

    PositionManager posMgr = new PositionManager(in);

    String input = posMgr.extract(true, false, false);

    int padNeeded = calculatePadNeeded(input, RADIX);
    if (padNeeded > 0) {
      input = addPadding(input, padNeeded, padding, RADIX);
    }

    String encrypted = encrypt(input, key, tweak, RADIX);

    // adjust offsets if padding was added to the front
    // no adjustment needed if padding was added to the back
    int padAdjust = padNeeded > 0 && padding == Pad.FRONT ? padNeeded : 0;
    String output = replaceSymbols(posMgr, encrypted, padAdjust, true, false, false);

    return output;
  }
}
