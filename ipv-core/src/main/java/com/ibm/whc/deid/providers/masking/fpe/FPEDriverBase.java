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

/**
 * Superclass with utility functions useful for FPE operations.
 */
public abstract class FPEDriverBase implements FPEDriver {

  /**
   * Determines the number of additional characters that would be needed to reach the minimum number
   * of input characters required for the given encryption radix.
   * 
   * @param input the non-null input characters available for encryption
   * 
   * @param radix the non-null radix that defines the input characters that can be encrypted for the
   *        current operation
   * 
   * @return the number of additional characters required, which will be 0 if the given input
   *         already consists of a sufficient number of characters
   * 
   * @throws UnsupportedLengthException if the given input already consists of more characters than
   *         the encryption can support
   */
  protected int calculatePadNeeded(String input, Radix radix) throws UnsupportedLengthException {
    int length = input.length();
    if (length > radix.getMaxStringLength()) {
      throw new UnsupportedLengthException(length, radix.getMinStringLength(),
          radix.getMaxStringLength());
    }
    return Math.max(radix.getMinStringLength() - length, 0);
  }

  /**
   * Adds additional characters as necessary to the input string so that the number of characters
   * meets the minimum number required by the encryption engine for the current operation.
   * 
   * @param input the non-null current encryption input
   * @param padNeeded the number of characters to add
   * @param padding the non-null type of padding request
   * @param radix the non-null radix that defines the input characters that can be encrypted for the
   *        current operation
   * 
   * @return the possibly-modified input string
   * 
   * @throws UnsupportedLengthException
   */
  protected String addPadding(String input, int padNeeded, Pad padding, Radix radix)
      throws UnsupportedLengthException {
    StringBuilder buffer = new StringBuilder(radix.getMinStringLength());
    switch (padding) {
      case NONE:
        throw new UnsupportedLengthException(input.length(), radix.getMinStringLength(),
          radix.getMaxStringLength());
      case FRONT:
        for (int i = 0; i < padNeeded; i++) {
          buffer.append(radix.getPadChar());
        }
        buffer.append(input);
        break;
      case BACK:
        buffer.append(input);
        for (int i = 0; i < padNeeded; i++) {
          buffer.append(radix.getPadChar());
        }
        break;
      default:
        throw new RuntimeException("unexpected Pad value " + padding.name());
      }
    return buffer.toString();
  }

  /**
   * Calls the encryption engine to encrypt the given input.
   * 
   * @param input the non-null input to encrypt. The string can consist only of characters supported
   *        by the given radix.
   * @param key the primary encryption key
   * @param tweak the secondary encryption key
   * @param radix the non-null radix that defines the input characters that can be encrypted for the
   *        current operation
   * 
   * @return the encrypted result
   * 
   * @throws EncryptionEngineException if the encryption engine fails for any reason
   */
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

  /**
   * Re-inserts any non-encrypted characters from the original value into the encrypted value with
   * each such character at its original offset, possibly shifted if additional characters have been
   * added to the front of the original input value to meet the minimum necessary number of
   * characters for encryption.
   * 
   * @param padNeeded the number of padding characters added to the result to meet minimum
   *        encryption requirements, which will be 0 if no such characters were added
   * @param padding the style of padding that was requested if padding was needed
   * @param posMgr the object that manages the categorization of each character in the original
   *        input value
   * @param encrypted the encrypted result value
   * @param includeDigits <i>True</i> if digits in the original value were encrypted and
   *        <i>false</i> otherwise
   * @param includeLower <i>True</i> if lower case letters in the original value were encrypted and
   *        <i>false</i> otherwise
   * @param includeUpper <i>True</i> if upper case letters in the original value were encrypted and
   *        <i>false</i> otherwise
   * 
   * @return the encrypted value with any non-encrypted characters from the original re-inserted as
   *         described
   */
  protected String replaceSymbols(int padNeeded, Pad padding, PositionManager posMgr,
      String encrypted, boolean includeDigits, boolean includeLower, boolean includeUpper) {
    String base = "";
    String prefix = "";
    String suffix = "";
    if (padNeeded > 0) {
      if (padding == Pad.FRONT) {
        prefix = encrypted.substring(0, padNeeded);
        base = encrypted.substring(padNeeded);
      } else if (padding == Pad.BACK) {
        base = encrypted.substring(0, encrypted.length() - padNeeded); 
        suffix = encrypted.substring(encrypted.length() - padNeeded);
      }
    } else {
      base = encrypted;
    }
    String replaced = posMgr.replaceSymbols(base, includeDigits, includeLower, includeUpper);
    StringBuilder buffer = new StringBuilder(encrypted.length());
    buffer.append(prefix).append(replaced).append(suffix);
    String output = buffer.toString();
    return output;
  }
}
