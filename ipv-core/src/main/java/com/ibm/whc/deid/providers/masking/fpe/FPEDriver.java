/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.UsageType;

/**
 * Interface including factory method for processors responsible for format-preserving encryption
 * services for a specific type of input with specific output formatting.
 */
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
        driver = new FPEDriverLettersSensitive();
        break;
      case DIGITS_LETTERS_LOWER:
        driver = new FPEDriverDigitsLettersLower();
        break;
      case DIGITS_LETTERS_UPPER:
        driver = new FPEDriverDigitsLettersUpper();
        break;
      case DIGITS_LETTERS_INSENSITIVE_AS_LOWER:
        driver = new FPEDriverDigitsLettersInsensitive(OutputCase.LOWER);
        break;
      case DIGITS_LETTERS_INSENSITIVE_AS_UPPER:
        driver = new FPEDriverDigitsLettersInsensitive(OutputCase.UPPER);
        break;
      case DIGITS_LETTERS_SENSITIVE:
        driver = new FPEDriverDigitsLettersSensitive();
        break;
      default:
        throw new IllegalArgumentException(type.name());
    }
    return driver;
  }

  /**
   * Apply FPE upon the given input.
   * 
   * @param in the input to encrypt and format
   * @param key the primary encryption key
   * @param tweak the secondary encryption key
   * @param padding strategy for data underflow
   * 
   * @return the encrypted and formatted result
   * 
   * @throws UnsupportedLengthException if the number of characters in the input that need to be
   *         encrypted is not within the range supported by the required encryption strategy. This
   *         occurs when too many characters need to be encrypted or when too few characters are
   *         presented for encryption and padding is either not supported or not requested.
   * 
   * @throws EncryptionEngineException if the encryption engine fails even though the input data is
   *         valid
   */
  public String encrypt(String in, String key, String tweak, Pad padding)
      throws UnsupportedLengthException, EncryptionEngineException;
}
