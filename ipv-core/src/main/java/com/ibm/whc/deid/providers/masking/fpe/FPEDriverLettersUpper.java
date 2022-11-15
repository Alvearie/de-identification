/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

/**
 * Concrete subclass for processing sequences of upper case letters.
 */
public class FPEDriverLettersUpper extends FPEDriverBaseSinglePhase {

  @Override
  protected boolean isEncryptDigits() {
    return false;
  }

  @Override
  protected boolean isEncryptLower() {
    return false;
  }

  @Override
  protected boolean isEncryptUpper() {
    return true;
  }

  @Override
  protected Radix getRadix() {
    return Radix.UPPER;
  }
}
