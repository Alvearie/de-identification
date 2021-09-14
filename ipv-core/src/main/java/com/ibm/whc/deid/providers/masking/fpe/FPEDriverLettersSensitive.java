/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import java.util.Locale;
import com.ibm.whc.deid.providers.masking.fpe.PositionManager.Position;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;

public class FPEDriverLettersSensitive extends FPEDriverBase {

  @Override
  public String encrypt(String in, String key, String tweak, Pad padding)
      throws UnsupportedLengthException, EncryptionEngineException {

    PositionManager posMgr = new PositionManager(in);

    String lowerInput = posMgr.extract(false, true, false);
    String upperInput = posMgr.extract(false, false, true);

    // no padding supported
    // verify both sets of input are of suitable length
    // method itself checks for too much input
    int padNeeded = calculatePadNeeded(lowerInput, Radix.LOWER);
    if (padNeeded > 0) {
      // TODO: log message
      throw new UnsupportedLengthException(padNeeded);
    }
    padNeeded = calculatePadNeeded(upperInput, Radix.LOWER);
    if (padNeeded > 0) {
      // TODO: log message
      throw new UnsupportedLengthException(padNeeded);
    }

    lowerInput = shiftLettersToBase26(lowerInput);
    upperInput = shiftLettersToBase26(upperInput.toLowerCase(Locale.US));

    String lowerEncrypted = encrypt(lowerInput, key, tweak, Radix.LOWER);
    String upperEncrypted = encrypt(upperInput, key, tweak, Radix.LOWER);

    String lowerResult = shiftBase26ToLetters(lowerEncrypted);
    String upperResult = shiftBase26ToLetters(upperEncrypted).toUpperCase(Locale.US);

    int lowerResultIndex = 0;
    int upperResultIndex = 0;
    StringBuilder buffer = new StringBuilder(in.length());
    for (Position p : posMgr.getPositions()) {
      switch (p.getType()) {
        case LOWER:
          buffer.append(lowerResult.charAt(lowerResultIndex++));
          break;
        case UPPER:
          buffer.append(upperResult.charAt(upperResultIndex++));
          break;
        default:
          buffer.append(p.getOriginal());
          break;
      }
    }
    return buffer.toString();
  }
}
