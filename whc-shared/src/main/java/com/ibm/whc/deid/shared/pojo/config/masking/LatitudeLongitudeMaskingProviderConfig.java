/*
 * (C) Copyright IBM Corp. 2016,2021
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
 * Masks latitude / longitude pairs, recognizing several location formats.
 */
@JsonInclude(Include.NON_NULL)
public class LatitudeLongitudeMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 1031774920782870184L;

  public static final int MINIMUM_OFFSET = 10;

  private boolean maskFixedRadiusRandomDirection = false;
  private boolean maskDonutMasking = false;
  private boolean maskRandomWithinCircle = true;
  private int offsetMaximumRadius = 100;
  private int offsetMinimumRadius = 50;

  public LatitudeLongitudeMaskingProviderConfig() {
    type = MaskingProviderType.LATITUDE_LONGITUDE;
  }

  public boolean isMaskFixedRadiusRandomDirection() {
    return maskFixedRadiusRandomDirection;
  }

  public void setMaskFixedRadiusRandomDirection(boolean maskFixedRadiusRandomDirection) {
    this.maskFixedRadiusRandomDirection = maskFixedRadiusRandomDirection;
  }

  public boolean isMaskDonutMasking() {
    return maskDonutMasking;
  }

  public void setMaskDonutMasking(boolean maskDonutMasking) {
    this.maskDonutMasking = maskDonutMasking;
  }

  public boolean isMaskRandomWithinCircle() {
    return maskRandomWithinCircle;
  }

  public void setMaskRandomWithinCircle(boolean maskRandomWithinCircle) {
    this.maskRandomWithinCircle = maskRandomWithinCircle;
  }

  public int getOffsetMaximumRadius() {
    return offsetMaximumRadius;
  }

  public void setOffsetMaximumRadius(int offsetMaximumRadius) {
    this.offsetMaximumRadius = offsetMaximumRadius;
  }

  public int getOffsetMinimumRadius() {
    return offsetMinimumRadius;
  }

  public void setOffsetMinimumRadius(int offsetMinimumRadius) {
    this.offsetMinimumRadius = offsetMinimumRadius;
  }

  @Override
  public void validate(DeidMaskingConfig maskingConfig)
      throws InvalidMaskingConfigurationException {
    super.validate(maskingConfig);
    if (offsetMaximumRadius <= MINIMUM_OFFSET) {
      throw new InvalidMaskingConfigurationException(
          "`offsetMaximumRadius` must be greater than " + MINIMUM_OFFSET);
    }
    if (offsetMinimumRadius <= MINIMUM_OFFSET) {
      throw new InvalidMaskingConfigurationException(
          "`offsetMinimumRadius` must be greater than " + MINIMUM_OFFSET);
    }
  }


  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (maskDonutMasking ? 1231 : 1237);
    result = prime * result + (maskFixedRadiusRandomDirection ? 1231 : 1237);
    result = prime * result + (maskRandomWithinCircle ? 1231 : 1237);
    result = prime * result + offsetMaximumRadius;
    result = prime * result + offsetMinimumRadius;
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
    LatitudeLongitudeMaskingProviderConfig other = (LatitudeLongitudeMaskingProviderConfig) obj;
    if (maskDonutMasking != other.maskDonutMasking)
      return false;
    if (maskFixedRadiusRandomDirection != other.maskFixedRadiusRandomDirection)
      return false;
    if (maskRandomWithinCircle != other.maskRandomWithinCircle)
      return false;
    if (offsetMaximumRadius != other.offsetMaximumRadius)
      return false;
    if (offsetMinimumRadius != other.offsetMinimumRadius)
      return false;
    return true;
  }
}
