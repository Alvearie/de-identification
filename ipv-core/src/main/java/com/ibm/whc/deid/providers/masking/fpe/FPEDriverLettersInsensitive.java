/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import java.util.Locale;
import com.ibm.whc.deid.providers.masking.fpe.PositionManager.CharType;
import com.ibm.whc.deid.providers.masking.fpe.PositionManager.Position;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;

public class FPEDriverLettersInsensitive extends FPEDriverBaseSinglePhase {

  private OutputCase outputCase;

  public FPEDriverLettersInsensitive(OutputCase outputCase) {
    this.outputCase = outputCase;
  }

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
    String outLetters = shiftBase26ToLetters(out);
    switch (outputCase) {
      case LOWER:
        break;
      case UPPER:
        outLetters = outLetters.toUpperCase(Locale.US);
        break;
      case ORIGINAL:
        StringBuilder buffer = new StringBuilder(outLetters);
        int index = 0;
        // skip pad characters
        if (padNeeded > 0 && padding == Pad.FRONT) {
          index = padNeeded;
        }
        for (Position pos : posMgr.getPositions()) {
          if (pos.getType() == CharType.UPPER) {
            // capitalize current position in buffer
            buffer.setCharAt(index, Character.toUpperCase(buffer.charAt(index)));
            index++;
          } else if (pos.getType() == CharType.LOWER) {
            // characters are already lower case
            index++;
          }
        }
        outLetters = buffer.toString();
        break;
    }
    return outLetters;
  }
}
