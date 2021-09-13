/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;
import com.privacylogistics.FF3Cipher;

public abstract class FPEDriverBase implements FPEDriver {

  protected int calculatePadNeeded(String input, Radix radix) throws UnsupportedLengthException {
    int length = input.length();
    if (length > radix.getMaxStringLength()) {
      throw new UnsupportedLengthException(length);
    }
    return Math.max(radix.getMinStringLength() - length, 0);
  }

  protected String addPadding(String input, int padNeeded, Pad padding, Radix radix)
      throws UnsupportedLengthException {
    if (padding == Pad.NONE) {
      // TODO: log message?
      throw new UnsupportedLengthException(input.length());
    }
    StringBuilder buffer = new StringBuilder(radix.getMinStringLength());
    if (padding == Pad.FRONT) {
      for (int i=0; i < padNeeded; i++) {
        buffer.append(radix.getPadChar());
      }
      buffer.append(input);
    } else { // Pad.BACK
      buffer.append(input);
      for (int i=0; i < padNeeded; i++) {
        buffer.append(radix.getPadChar());
      }
    }
    return buffer.toString();
  }

  protected String encrypt(String input, String key, String tweak, Radix radix)
      throws EncryptionEngineException {
    try {
      FF3Cipher cipher = new FF3Cipher(key, tweak, radix.value());
      String encrypted = cipher.encrypt(input);
      return encrypted;
    } catch (BadPaddingException | IllegalBlockSizeException e) {
      throw new EncryptionEngineException(e);
    }
  }

  protected String replaceSymbols(PositionManager posMgr, String encrypted, int padAdjust,
      boolean includeDigits, boolean includeLower, boolean includeUpper) {
    String prefix = encrypted.substring(0, padAdjust);
    String base = encrypted.substring(padAdjust);
    String replaced = posMgr.replaceSymbols(base, includeDigits, includeLower, includeUpper);
    String output = padAdjust > 0 ? prefix + replaced : replaced;
    return output;
  }
}
