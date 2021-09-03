/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/**
 * Configuration for the Format-Preserving Encryption privacy provider.
 */
@JsonInclude(Include.NON_NULL)
public class FPEMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 1L;

  /**
   * The characters that will be encrypted.
   */
  public enum Radix {

    /**
     * Digits 0-9
     */
    DIGITS(10, 6, 56, '0'),

    /**
     * lower case letters a-z
     */
    LOWER(26, 5, 40, 'a'),

    /**
     * Digits 0-9 or lower case letters a-z
     */
    DIGITS_LOWER(36, 4, 36, 'a');

    private final int value;
    private final int minchars;
    private final int maxchars;
    private final char padchar;

    private Radix(int val, int min, int max, char pad) {
      value = val;
      minchars = min;
      maxchars = max;
      padchar = pad;
    }

    public int value() {
      return value;
    }

    public int getMinStringLength() {
      return minchars;
    }

    public int getMaxStringLength() {
      return maxchars;
    }
    
    public char getPadChar() {
      return padchar;
    }
  }

  public enum Pad {

    /**
     * No padding - error if value has less than the minimum characters for the radix
     */
    NONE,

    /**
     * add pad characters as necessary to the beginning of the value
     */
    FRONT,

    /**
     * add pad characters as necessary to the end of the value
     */
    BACK
  }

  public static final Radix DEFAULT_RADIX = Radix.DIGITS;
  public static final Pad DEFAULT_PAD = Pad.NONE;

  private Radix radix;
  private Pad pad;
  private boolean preserveSymbolLocation = true;
  private boolean caseInsensitive = false;

  public FPEMaskingProviderConfig() {
    radix = DEFAULT_RADIX;
    pad = DEFAULT_PAD;
    type = MaskingProviderType.FPE;
  }

  public Radix getRadix() {
    return radix;
  }

  public void setRadix(Radix radix) {
    this.radix = radix == null ? DEFAULT_RADIX : radix;
  }

  public Pad getPad() {
    return pad;
  }

  public void setPad(Pad pad) {
    this.pad = pad == null ? DEFAULT_PAD : pad;
  }

  public boolean isPreserveSymbolLocation() {
    return preserveSymbolLocation;
  }

  public void setPreserveSymbolLocation(boolean preserveSymbolLocation) {
    this.preserveSymbolLocation = preserveSymbolLocation;
  }

  public boolean isCaseInsensitive() {
    return caseInsensitive;
  }

  public void setCaseInsensitive(boolean caseInsensitive) {
    this.caseInsensitive = caseInsensitive;
  }

  @Override
  public void validate(DeidMaskingConfig maskingConfig)
      throws InvalidMaskingConfigurationException {
    super.validate(maskingConfig);
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + Objects.hash(caseInsensitive, pad, preserveSymbolLocation, radix);
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (!super.equals(obj)) {
      return false;
    }
    if (!(obj instanceof FPEMaskingProviderConfig)) {
      return false;
    }
    FPEMaskingProviderConfig other = (FPEMaskingProviderConfig) obj;
    return caseInsensitive == other.caseInsensitive && pad == other.pad
        && preserveSymbolLocation == other.preserveSymbolLocation && radix == other.radix;
  }
}
