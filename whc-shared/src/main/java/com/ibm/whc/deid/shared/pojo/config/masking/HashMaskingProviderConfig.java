/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;

/*
 * Hashes the original value using an algorithm
 */
@JsonInclude(Include.NON_NULL)
public class HashMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -7825324395792039252L;

  private String algorithmDefault = "SHA-256";
  private boolean offsetOffsetMask = false;
  private boolean offsetOffsetMaskDelete = false;
  private int offsetBegin = -1;
  private int offsetEnd = -1;
  private String salt = "";
  private int offsetInvalidOffsetValue = 1;

  public HashMaskingProviderConfig() {
    type = MaskingProviderType.HASH;
  }

  public String getAlgorithmDefault() {
    return algorithmDefault;
  }

  public void setAlgorithmDefault(String algorithm) {
    this.algorithmDefault = algorithm;
  }

  public boolean isOffsetOffsetMask() {
    return offsetOffsetMask;
  }

  public void setOffsetOffsetMask(boolean offsetMask) {
    this.offsetOffsetMask = offsetMask;
  }

  public boolean isOffsetOffsetMaskDelete() {
    return offsetOffsetMaskDelete;
  }

  public void setOffsetOffsetMaskDelete(boolean offsetMaskDelete) {
    this.offsetOffsetMaskDelete = offsetMaskDelete;
  }

  public int getOffsetBegin() {
    return offsetBegin;
  }

  public void setOffsetBegin(int offsetBegin) {
    this.offsetBegin = offsetBegin;
  }

  public int getOffsetEnd() {
    return offsetEnd;
  }

  public void setOffsetEnd(int offsetEnd) {
    this.offsetEnd = offsetEnd;
  }

  public int getOffsetInvalidOffsetValue() {
    return offsetInvalidOffsetValue;
  }

  public void setOffsetInvalidOffsetValue(int invalidOffsetValue) {
    this.offsetInvalidOffsetValue = invalidOffsetValue;
  }

  public String getSalt() {
    return salt;
  }

  public void setSalt(String salt) {
    this.salt = salt == null ? "" : salt;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((algorithmDefault == null) ? 0 : algorithmDefault.hashCode());
    result = prime * result + offsetBegin;
    result = prime * result + offsetEnd;
    result = prime * result + offsetInvalidOffsetValue;
    result = prime * result + (offsetOffsetMask ? 1231 : 1237);
    result = prime * result + (offsetOffsetMaskDelete ? 1231 : 1237);
    result = prime * result + ((salt == null) ? 0 : salt.hashCode());
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
    HashMaskingProviderConfig other = (HashMaskingProviderConfig) obj;
    if (algorithmDefault == null) {
      if (other.algorithmDefault != null)
        return false;
    } else if (!algorithmDefault.equals(other.algorithmDefault))
      return false;
    if (offsetBegin != other.offsetBegin)
      return false;
    if (offsetEnd != other.offsetEnd)
      return false;
    if (offsetInvalidOffsetValue != other.offsetInvalidOffsetValue)
      return false;
    if (offsetOffsetMask != other.offsetOffsetMask)
      return false;
    if (offsetOffsetMaskDelete != other.offsetOffsetMaskDelete)
      return false;
    if (salt == null) {
      if (other.salt != null)
        return false;
    } else if (!salt.equals(other.salt))
      return false;
    return true;
  }
}
