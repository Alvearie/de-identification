/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;

/**
 * Superclass with template implementation for FPE encryption in which all the characters to be
 * encrypted in the original value can be encrypted together as a single character set with one call
 * to the encryption engine.
 */
public abstract class FPEDriverBaseSinglePhase extends FPEDriverBase {

  @Override
  public String encrypt(String in, String key, String tweak, Pad padding)
      throws UnsupportedLengthException, EncryptionEngineException {
    String output;

    PositionManager posMgr = new PositionManager(in);

    String input = posMgr.extract(isEncryptDigits(), isEncryptLower(), isEncryptUpper());

    if (input.isEmpty()) {
      // nothing to encrypt, just return original value
      output = in;

    } else {
      int padNeeded = calculatePadNeeded(input, getRadix());
      if (padNeeded > 0) {
        input = addPadding(input, padNeeded, padding, getRadix());
      }

      input = processInput(input, posMgr, padNeeded, padding);

      String encrypted = encrypt(input, key, tweak, getRadix());

      encrypted = processOutput(encrypted, posMgr, padNeeded, padding);

      output = replaceSymbols(padNeeded, padding, posMgr, encrypted, isEncryptDigits(),
          isEncryptLower(), isEncryptUpper());
    }

    return output;
  }

  /**
   * 
   * @return <i>True</i> if digits in the original value are to be encrypted and <i>false</i>
   *         otherwise
   */
  protected abstract boolean isEncryptDigits();

  /**
   * 
   * @return <i>True</i> if lower case letters in the original value are to be encrypted and
   *         <i>false</i> otherwise
   */
  protected abstract boolean isEncryptLower();

  /**
   * 
   * @return <i>True</i> if upper case letters in the original value are to be encrypted and
   *         <i>false</i> otherwise
   */
  protected abstract boolean isEncryptUpper();

  /**
   * 
   * @return the radix that defines the input characters that can be encrypted for the current
   *         operation
   */
  protected abstract Radix getRadix();

  /**
   * Stub method for any required post-processing of the input string that is to be encrypted before
   * calling the encryption engine. This implementation does nothing. Concrete subclasses might
   * override this value.
   * 
   * @param in the input to be encrypted, any characters in the original value that are not to be
   *        encrypted have been removed from this value
   * @param posMgr the object that manages the categorization of each character in the original
   *        input value
   * @param padNeeded the number padding characters that were added to the input to meet minimum
   *        requirements for the encryption
   * @param padding the style of padding that was performed if padding was required
   * 
   * @return the adjusted encryption input value
   */
  protected String processInput(String in, PositionManager posMgr, int padNeeded, Pad padding) {
    // nothing required - use input as-is
    return in;
  }

  /**
   * Stub method for any required post-processing of the encrypted value before it is returned to
   * the caller. This implementation does nothing. Concrete subclasses might override this value.
   * 
   * @param out the encrypted output string. Any characters in the original value that were not
   *        encrypted have not yet been added back to this string. This method is not responsible
   *        for adding any such characters.
   * @param posMgr the object that manages the categorization of each character in the original
   *        input value
   * @param padNeeded the number padding characters that were added to the input to meet minimum
   *        requirements for the encryption
   * @param padding the style of padding that was performed if padding was required
   * 
   * @return the adjusted encryption output value
   */
  protected String processOutput(String out, PositionManager posMgr, int padNeeded, Pad padding) {
    // nothing required - use output as-is
    return out;
  }
}
