/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import com.ibm.whc.deid.providers.masking.fpe.PositionManager.CharType;
import com.ibm.whc.deid.providers.masking.fpe.PositionManager.Position;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;

public class FPEDriverDigitsLettersSensitive extends FPEDriverLettersSensitive {

  @Override
  public String encrypt(String in, String key, String tweak, Pad padding)
      throws UnsupportedLengthException, EncryptionEngineException {

    PositionManager posMgr = new PositionManager(in);

    String lowerResult = getEncryptedChars(key, tweak, posMgr, Radix.LOWER, CharType.LOWER);
    String upperResult = getEncryptedChars(key, tweak, posMgr, Radix.LOWER, CharType.UPPER);
    String digitsResult = getEncryptedChars(key, tweak, posMgr, Radix.DIGITS, CharType.DIGIT);


    int lowerResultIndex = 0;
    int upperResultIndex = 0;
    int digitsResultIndex = 0;
    StringBuilder buffer = new StringBuilder(in.length());
    for (Position p : posMgr.getPositions()) {
      switch (p.getType()) {
        case LOWER:
          buffer.append(lowerResult.charAt(lowerResultIndex++));
          break;
        case UPPER:
          buffer.append(upperResult.charAt(upperResultIndex++));
          break;
        case DIGIT:
          buffer.append(digitsResult.charAt(digitsResultIndex++));
          break;
        default:
          buffer.append(p.getOriginal());
          break;
      }
    }
    return buffer.toString();
  }
}
