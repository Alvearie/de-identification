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
 * Masks a numeric data value by adding a random offset.
 */
@JsonInclude(Include.NON_NULL)
public class NumberVarianceMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -7424497174746790486L;
  private boolean augmentMask = false;
  private double augmentLowerBound = 1.0;
  private double augmentUpperBound = 10.0;
  private boolean resultWithPrecision = false;
  private int precisionDigits = -1;
  private double maskLimitUp = 10.0;
  private double maskLimitDown = 10.0;

  public NumberVarianceMaskingProviderConfig() {
    type = MaskingProviderType.NUMBERVARIANCE;
  }

  public boolean isAugmentMask() {
    return augmentMask;
  }

  public void setAugmentMask(boolean augmentMask) {
    this.augmentMask = augmentMask;
  }

  public double getAugmentLowerBound() {
    return augmentLowerBound;
  }

  public void setAugmentLowerBound(double augmentLowerBound) {
    this.augmentLowerBound = augmentLowerBound;
  }

  public double getAugmentUpperBound() {
    return augmentUpperBound;
  }

  public void setAugmentUpperBound(double augmentUpperBound) {
    this.augmentUpperBound = augmentUpperBound;
  }

  public boolean isResultWithPrecision() {
    return resultWithPrecision;
  }

  public void setResultWithPrecision(boolean resultWithPrecision) {
    this.resultWithPrecision = resultWithPrecision;
  }

  public int getPrecisionDigits() {
    return precisionDigits;
  }

  public void setPrecisionDigits(int precisionDigits) {
    this.precisionDigits = precisionDigits;
  }

  public double getMaskLimitUp() {
    return maskLimitUp;
  }

  public void setMaskLimitUp(double maskLimitUp) {
    this.maskLimitUp = maskLimitUp;
  }

  public double getMaskLimitDown() {
    return maskLimitDown;
  }

  public void setMaskLimitDown(double maskLimitDown) {
    this.maskLimitDown = maskLimitDown;
  }

  @Override
  public void validate() throws InvalidMaskingConfigurationException {
    if (augmentLowerBound < 0.0) {
      throw new InvalidMaskingConfigurationException("`augmentLowerBound` must be greater than 0");
    }
    if (augmentUpperBound < 0.0) {
      throw new InvalidMaskingConfigurationException("`augmentUpperBound` must be greater than 0");
    }
    if (augmentMask && augmentLowerBound > augmentUpperBound) {
      throw new InvalidMaskingConfigurationException(
          "`augmentLowerBound` must be less than `augmentUpperBound` when `augmentMask` is true");
    }
    if (augmentMask && augmentLowerBound == augmentUpperBound && !resultWithPrecision) {
      throw new InvalidMaskingConfigurationException(
          "`augmentUpperBound` must not be equal to `augmentLowerBound` when `augmentMask` is true");
    }
  }
    
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    long temp;
    temp = Double.doubleToLongBits(augmentLowerBound);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    result = prime * result + (augmentMask ? 1231 : 1237);
    temp = Double.doubleToLongBits(augmentUpperBound);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(maskLimitDown);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    temp = Double.doubleToLongBits(maskLimitUp);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    result = prime * result + precisionDigits;
    result = prime * result + (resultWithPrecision ? 1231 : 1237);
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
    NumberVarianceMaskingProviderConfig other = (NumberVarianceMaskingProviderConfig) obj;
    if (Double.doubleToLongBits(augmentLowerBound) != Double
        .doubleToLongBits(other.augmentLowerBound))
      return false;
    if (augmentMask != other.augmentMask)
      return false;
    if (Double.doubleToLongBits(augmentUpperBound) != Double
        .doubleToLongBits(other.augmentUpperBound))
      return false;
    if (Double.doubleToLongBits(maskLimitDown) != Double.doubleToLongBits(other.maskLimitDown))
      return false;
    if (Double.doubleToLongBits(maskLimitUp) != Double.doubleToLongBits(other.maskLimitUp))
      return false;
    if (precisionDigits != other.precisionDigits)
      return false;
    if (resultWithPrecision != other.resultWithPrecision)
      return false;
    return true;
  }
}
