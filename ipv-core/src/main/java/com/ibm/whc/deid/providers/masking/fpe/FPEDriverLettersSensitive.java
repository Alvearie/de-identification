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

/**
 * Class for processing sequences containing any lower case letters and upper case letters as
 * separate domains. Any input characters in these two groups are extracted, concatenated, and used
 * as an input to the encryption engine, therefore two separate encryption calls are possible. After
 * encryption the results are used to reconstruct an output value in the same format as the
 * original.
 */
public class FPEDriverLettersSensitive extends FPEDriverBase {

  @Override
  public String encrypt(String in, String key, String tweak, Pad padding)
      throws UnsupportedLengthException, EncryptionEngineException {

    PositionManager posMgr = new PositionManager(in);

    String lowerResult = getEncryptedChars(key, tweak, posMgr, Radix.LOWER, CharType.LOWER);
    String upperResult = getEncryptedChars(key, tweak, posMgr, Radix.LOWER, CharType.UPPER);

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

  protected String getEncryptedChars(String key, String tweak, PositionManager posMgr, Radix radix,
      CharType charType) throws UnsupportedLengthException, EncryptionEngineException {

    String result;

    String input = posMgr.extract(charType == CharType.DIGIT, charType == CharType.LOWER,
        charType == CharType.UPPER);

    if (input.isEmpty()) {
      result = "";

    } else {
      // no padding supported
      // verify both sets of input are of suitable length
      // method itself checks for too much input
      int padNeeded = calculatePadNeeded(input, radix);
      if (padNeeded > 0) {
        throw new UnsupportedLengthException(input.length(), radix.getMinStringLength(),
            radix.getMaxStringLength());
      }

      if (charType == CharType.UPPER) {
        input = input.toLowerCase(Locale.US);
      }
      if (charType == CharType.LOWER || charType == CharType.UPPER) {
        input = shiftLettersToBase26(input);
      }
      result = encrypt(input, key, tweak, radix);
      if (charType == CharType.LOWER || charType == CharType.UPPER) {
        result = shiftBase26ToLetters(result);
      }
      if (charType == CharType.UPPER) {
        result = result.toUpperCase(Locale.US);
      }
    }
    return result;
  }
}
