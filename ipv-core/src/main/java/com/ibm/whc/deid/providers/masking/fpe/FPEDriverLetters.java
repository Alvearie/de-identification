/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;

public class FPEDriverLetters extends FPEDriverBase {

  private Radix radix;
  private boolean encryptsDigits;
  private boolean encryptsLower;
  private boolean encryptsUpper;

  public FPEDriverLetters(Radix radix, boolean encryptsDigits, boolean encryptsLower,
      boolean encryptsUpper) {
    this.radix = radix;
    this.encryptsDigits = encryptsDigits;
    this.encryptsLower = encryptsLower;
    this.encryptsUpper = encryptsUpper;
  }

  @Override
  public String encrypt(String in, String key, String tweak, Pad padding)
      throws UnsupportedLengthException, EncryptionEngineException {

    PositionManager posMgr = new PositionManager(in);

    String input = posMgr.extract(encryptsDigits, encryptsLower, encryptsUpper);
    input = shiftLettersToBase26(input);

    int padNeeded = calculatePadNeeded(input, radix);
    if (padNeeded > 0) {
      input = addPadding(input, padNeeded, padding, radix);
    }

    String encrypted = encrypt(input, key, tweak, radix);
    encrypted = shiftBase26ToLetters(encrypted);

    // adjust offsets if padding was added to the front
    // no adjustment needed if padding was added to the back
    int padAdjust = padNeeded > 0 && padding == Pad.FRONT ? padNeeded : 0;
    String output =
        replaceSymbols(posMgr, encrypted, padAdjust, encryptsDigits, encryptsLower, encryptsUpper);

    return output;
  }
}
