/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/*
 * Masks original zip code values while offering a number of utility-preserving options
 */
@JsonInclude(Include.NON_NULL)
public class ZIPCodeMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 2253187136824335936L;
  public static final Integer MASK_PREFIX_LENGTH_DEFAULT = Integer.valueOf(3);
  private String maskCountryCode = "US";
  private boolean maskReplaceWithNeighbor = false;
  private int maskReplaceWithNeighborNearestCount = 10;
  private int maskPrefixLength = MASK_PREFIX_LENGTH_DEFAULT.intValue();
  private boolean maskPrefixRequireMinPopulation = false;
  private int maskPrefixMinPopulation = 20000;
  private boolean maskTruncateIfNotMinPopulation = false;
  private int maskTruncateLengthIfNotMinPopulation = 2;
  private boolean maskSuffixTruncate = true;
  private boolean maskSuffixReplaceWithRandom = false;
  private boolean maskSuffixReplaceWithValidOnly = false;

  public ZIPCodeMaskingProviderConfig() {
    type = MaskingProviderType.ZIPCODE;
  }

  public String getMaskCountryCode() {
    return maskCountryCode;
  }

  public void setMaskCountryCode(String maskCountryCode) {
    this.maskCountryCode = maskCountryCode;
  }

  public boolean isMaskReplaceWithNeighbor() {
    return maskReplaceWithNeighbor;
  }

  public void setMaskReplaceWithNeighbor(boolean maskReplaceWithNeighbor) {
    this.maskReplaceWithNeighbor = maskReplaceWithNeighbor;
  }

  public int getMaskReplaceWithNeighborNearestCount() {
    return maskReplaceWithNeighborNearestCount;
  }

  public void setMaskReplaceWithNeighborNearestCount(int maskReplaceWithNeighborNearestCount) {
    this.maskReplaceWithNeighborNearestCount = maskReplaceWithNeighborNearestCount;
  }

  public int getMaskPrefixLength() {
    return maskPrefixLength;
  }

  public void setMaskPrefixLength(int maskPrefixLength) {
    this.maskPrefixLength = maskPrefixLength;
  }

  public boolean isMaskPrefixRequireMinPopulation() {
    return maskPrefixRequireMinPopulation;
  }

  public void setMaskPrefixRequireMinPopulation(boolean maskPrefixRequireMinPopulation) {
    this.maskPrefixRequireMinPopulation = maskPrefixRequireMinPopulation;
  }

  public int getMaskPrefixMinPopulation() {
    return maskPrefixMinPopulation;
  }

  public void setMaskPrefixMinPopulation(int maskPrefixMinPopulation) {
    this.maskPrefixMinPopulation = maskPrefixMinPopulation;
  }

  public boolean isMaskTruncateIfNotMinPopulation() {
    return maskTruncateIfNotMinPopulation;
  }

  public void setMaskTruncateIfNotMinPopulation(boolean maskTruncateIfNotMinPopulation) {
    this.maskTruncateIfNotMinPopulation = maskTruncateIfNotMinPopulation;
  }

  public int getMaskTruncateLengthIfNotMinPopulation() {
    return maskTruncateLengthIfNotMinPopulation;
  }

  public void setMaskTruncateLengthIfNotMinPopulation(int maskTruncateLengthIfNotMinPopulation) {
    this.maskTruncateLengthIfNotMinPopulation = maskTruncateLengthIfNotMinPopulation;
  }

  public boolean isMaskSuffixTruncate() {
    return maskSuffixTruncate;
  }

  public void setMaskSuffixTruncate(boolean maskSuffixTruncate) {
    this.maskSuffixTruncate = maskSuffixTruncate;
  }

  public boolean isMaskSuffixReplaceWithRandom() {
    return maskSuffixReplaceWithRandom;
  }

  public void setMaskSuffixReplaceWithRandom(boolean maskSuffixReplaceWithRandom) {
    this.maskSuffixReplaceWithRandom = maskSuffixReplaceWithRandom;
  }

  public boolean isMaskSuffixReplaceWithValidOnly() {
    return maskSuffixReplaceWithValidOnly;
  }

  public void setMaskSuffixReplaceWithValidOnly(boolean maskSuffixReplaceWithValidOnly) {
    this.maskSuffixReplaceWithValidOnly = maskSuffixReplaceWithValidOnly;
  }

  @Override
  public void validate() throws InvalidMaskingConfigurationException {
    super.validate();
    if (maskCountryCode == null) {
      throw new InvalidMaskingConfigurationException(
          "`maskCountryCode` must not be null");
    }
    if (maskReplaceWithNeighborNearestCount < 1) {
      throw new InvalidMaskingConfigurationException(
          "`maskReplaceWithNeighborNearestCount` must be greater than 0");
    }
    if (maskPrefixLength < 1) {
      throw new InvalidMaskingConfigurationException(
          "`maskPrefixLength` must be greater than 0");
    }
    if (maskPrefixMinPopulation < 1) {
      throw new InvalidMaskingConfigurationException(
          "`maskPrefixMinPopulation` must be greater than 0");
    }
    if (maskTruncateLengthIfNotMinPopulation < 1) {
      throw new InvalidMaskingConfigurationException(
          "`maskTruncateLengthIfNotMinPopulation` must be greater than 0");
    }
  }


  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((maskCountryCode == null) ? 0 : maskCountryCode.hashCode());
    result = prime * result + maskPrefixLength;
    result = prime * result + maskPrefixMinPopulation;
    result = prime * result + (maskPrefixRequireMinPopulation ? 1231 : 1237);
    result = prime * result + (maskReplaceWithNeighbor ? 1231 : 1237);
    result = prime * result + maskReplaceWithNeighborNearestCount;
    result = prime * result + (maskSuffixReplaceWithRandom ? 1231 : 1237);
    result = prime * result + (maskSuffixReplaceWithValidOnly ? 1231 : 1237);
    result = prime * result + (maskSuffixTruncate ? 1231 : 1237);
    result = prime * result + (maskTruncateIfNotMinPopulation ? 1231 : 1237);
    result = prime * result + maskTruncateLengthIfNotMinPopulation;
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (!super.equals(obj))
      return false;
    if (getClass() != obj.getClass())
      return false;
    ZIPCodeMaskingProviderConfig other = (ZIPCodeMaskingProviderConfig) obj;
    if (maskCountryCode == null) {
      if (other.maskCountryCode != null)
        return false;
    } else if (!maskCountryCode.equals(other.maskCountryCode))
      return false;
    if (maskPrefixLength != other.maskPrefixLength)
      return false;
    if (maskPrefixMinPopulation != other.maskPrefixMinPopulation)
      return false;
    if (maskPrefixRequireMinPopulation != other.maskPrefixRequireMinPopulation)
      return false;
    if (maskReplaceWithNeighbor != other.maskReplaceWithNeighbor)
      return false;
    if (maskReplaceWithNeighborNearestCount != other.maskReplaceWithNeighborNearestCount)
      return false;
    if (maskSuffixReplaceWithRandom != other.maskSuffixReplaceWithRandom)
      return false;
    if (maskSuffixReplaceWithValidOnly != other.maskSuffixReplaceWithValidOnly)
      return false;
    if (maskSuffixTruncate != other.maskSuffixTruncate)
      return false;
    if (maskTruncateIfNotMinPopulation != other.maskTruncateIfNotMinPopulation)
      return false;
    if (maskTruncateLengthIfNotMinPopulation != other.maskTruncateLengthIfNotMinPopulation)
      return false;
    return true;
  }
}
