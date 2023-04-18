/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

/**
 * Concrete class for processing input numeric sequences.
 */
public class FPEDriverDigits extends FPEDriverBaseSinglePhase {

  @Override
  protected boolean isEncryptDigits() {
    return true;
  }

  @Override
  protected boolean isEncryptLower() {
    return false;
  }

  @Override
  protected boolean isEncryptUpper() {
    return false;
  }

  @Override
  protected Radix getRadix() {
    return Radix.DIGITS;
  }
}
