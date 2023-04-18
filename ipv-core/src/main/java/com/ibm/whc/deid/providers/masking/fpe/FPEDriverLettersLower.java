/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

/**
 * Concrete subclass for processing sequences of lower case letters.
 */
public class FPEDriverLettersLower extends FPEDriverBaseSinglePhase {

  @Override
  protected boolean isEncryptDigits() {
    return false;
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
    return Radix.LOWER;
  }
}
