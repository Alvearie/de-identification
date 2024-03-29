/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/*
 * Replaces an original value with either asterisks or with random characters.
 */
@JsonInclude(Include.NON_NULL)
public class ReplaceMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 1064469413046061087L;
  private boolean maskReplaceWithAsterisks = false;
  private int maskPreserve = 3;
  private int maskOffset = 0;
  private boolean maskReplaceWithRandom = false;

  public ReplaceMaskingProviderConfig() {
    type = MaskingProviderType.REPLACE;
  }

  public boolean isMaskReplaceWithAsterisks() {
    return maskReplaceWithAsterisks;
  }

  public void setMaskReplaceWithAsterisks(boolean maskReplaceWithAsterisks) {
    this.maskReplaceWithAsterisks = maskReplaceWithAsterisks;
  }

  public int getMaskPreserve() {
    return maskPreserve;
  }

  public void setMaskPreserve(int maskPreserve) {
    this.maskPreserve = maskPreserve;
  }

  public int getMaskOffset() {
    return maskOffset;
  }

  public void setMaskOffset(int maskOffset) {
    this.maskOffset = maskOffset;
  }

  public boolean isMaskReplaceWithRandom() {
    return maskReplaceWithRandom;
  }

  public void setMaskReplaceWithRandom(boolean maskReplaceWithRandom) {
    this.maskReplaceWithRandom = maskReplaceWithRandom;
  }

  @Override
  public void validate(DeidMaskingConfig maskingConfig)
      throws InvalidMaskingConfigurationException {
    super.validate(maskingConfig);
    if (maskOffset < 0) {
      throw new InvalidMaskingConfigurationException(
          "`maskOffset` must be greater than or equal to 0");
    }
    if (maskPreserve < 0) {
      throw new InvalidMaskingConfigurationException(
          "`maskPreserve` must be greater than or equal to 0");
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + maskOffset;
    result = prime * result + maskPreserve;
    result = prime * result + (maskReplaceWithAsterisks ? 1231 : 1237);
    result = prime * result + (maskReplaceWithRandom ? 1231 : 1237);
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
    ReplaceMaskingProviderConfig other = (ReplaceMaskingProviderConfig) obj;
    if (maskOffset != other.maskOffset)
      return false;
    if (maskPreserve != other.maskPreserve)
      return false;
    if (maskReplaceWithAsterisks != other.maskReplaceWithAsterisks)
      return false;
    if (maskReplaceWithRandom != other.maskReplaceWithRandom)
      return false;
    return true;
  }
}
