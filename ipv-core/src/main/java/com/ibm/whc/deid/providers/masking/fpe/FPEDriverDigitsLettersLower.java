/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

/**
 * Concrete subclass for processing sequences of digits and lower case letters.
 */
public class FPEDriverDigitsLettersLower extends FPEDriverBaseSinglePhase {

  @Override
  protected boolean isEncryptDigits() {
    return true;
  }

  @Override
  protected boolean isEncryptLower() {
    return true;
  }

  @Override
  protected boolean isEncryptUpper() {
    return false;
  }

  @Override
  protected Radix getRadix() {
    return Radix.DIGITS_LOWER;
  }
}
