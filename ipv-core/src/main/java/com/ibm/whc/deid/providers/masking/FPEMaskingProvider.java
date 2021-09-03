/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Radix;

/**
 * Masks identifiers as per NIST Format-Preserving Encryption FF3-1.
 */
public class FPEMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 1L;

  private final Radix radix;
  private final Pad pad;
  private final boolean preserveSymbolLocation;
  private final boolean caseInsensitive;

  protected class UnsupportedLengthException extends Exception {

    private static final long serialVersionUID = 1L;

    public UnsupportedLengthException(int length) {
      super(Integer.toString(length));
    }
  }

  /**
   * Instantiates a new FPE masking provider.
   *
   * @param config the configuration
   */
  public FPEMaskingProvider(FPEMaskingProviderConfig config) {
    super(config);
    this.radix = config.getRadix();
    this.pad = config.getPad();
    this.preserveSymbolLocation = config.isPreserveSymbolLocation();
    this.caseInsensitive = config.isCaseInsensitive();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    // TODO remove symbols

    String in;
    try {
      in = verifyLength(identifier);
    } catch (UnsupportedLengthException e) {
      // TODO: message
      return applyUnexpectedValueHandling(identifier, null);
    }

    FF3Cipher cipher = new FF3Cipher("jarrett-key", "jra-tweak", radix.value());
    
  }

  protected String verifyLength(String input) throws UnsupportedLengthException {
    String adjusted = input;
    int length = input.length();
    if (length > radix.getMaxStringLength()) {
      throw new UnsupportedLengthException(length);
    }
    int minlen = radix.getMinStringLength();
    if (length < minlen) {
      switch (pad) {
        case NONE:
          //TODO: message
          throw new UnsupportedLengthException(length);
        case FRONT:
        case BACK:
          char padchar = radix.getPadChar();
          StringBuilder buffer = new StringBuilder(radix.getMinStringLength());
          for (int i = 0; i < minlen - length; i++) {
            buffer.append(padchar);
          }
          if (pad == Pad.FRONT) {
            buffer.append(input);
          } else {
            buffer.insert(0, input);
          }
          adjusted = buffer.toString();
          break;
      }
    }
    return adjusted;
  }
}
