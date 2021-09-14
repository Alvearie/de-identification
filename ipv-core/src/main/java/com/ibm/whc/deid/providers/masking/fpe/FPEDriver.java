/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.UsageType;

public interface FPEDriver {

  public static FPEDriver getFPEDriver(UsageType type) {
    FPEDriver driver;
    switch (type) {
      case DIGITS:
        driver = new FPEDriverDigits();
        break;
      case LETTERS_LOWER:
        driver = new FPEDriverLettersLower();
        break;
      case LETTERS_UPPER:
        driver = new FPEDriverLettersUpper();
        break;
      case LETTERS_INSENSITIVE_AS_LOWER:
        driver = new FPEDriverLettersInsensitive(OutputCase.LOWER);
        break;
      case LETTERS_INSENSITIVE_AS_UPPER:
        driver = new FPEDriverLettersInsensitive(OutputCase.UPPER);
        break;
      case LETTERS_INSENSITIVE_AS_ORIGINAL:
        driver = new FPEDriverLettersInsensitive(OutputCase.ORIGINAL);
        break;
      case LETTERS_SENSITIVE:
      case DIGITS_LETTERS_LOWER:
      case DIGITS_LETTERS_UPPER:
      case DIGITS_LETTERS_INSENSITIVE_AS_LOWER:
      case DIGITS_LETTERS_INSENSITIVE_AS_UPPER:
      case DIGITS_LETTERS_SENSITIVE:
        // TODO: finish
        throw new RuntimeException("not yet implemented");
      default:
        throw new IllegalArgumentException(type.name());
    }
    return driver;
  }

  public String encrypt(String in, String key, String tweak, Pad padding)
      throws UnsupportedLengthException, EncryptionEngineException;
}
