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
 * Redacts a value by replacing it with a character (default is the letter X). The length of the
 * value can be optionally preserved.
 */
@JsonInclude(Include.NON_NULL)
public class RedactMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -1418940833877275858L;
  boolean preserveLength = true;
  String replaceCharacter = "X";

  public RedactMaskingProviderConfig() {
    type = MaskingProviderType.REDACT;
  }

  public boolean isPreserveLength() {
    return preserveLength;
  }

  public void setPreserveLength(boolean preserveLength) {
    this.preserveLength = preserveLength;
  }

  public String getReplaceCharacter() {
    return replaceCharacter;
  }

  public void setReplaceCharacter(String replaceCharacter) {
    this.replaceCharacter = replaceCharacter;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (preserveLength ? 1231 : 1237);
    result = prime * result + ((replaceCharacter == null) ? 0 : replaceCharacter.hashCode());
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
    RedactMaskingProviderConfig other = (RedactMaskingProviderConfig) obj;
    if (preserveLength != other.preserveLength)
      return false;
    if (replaceCharacter == null) {
      if (other.replaceCharacter != null)
        return false;
    } else if (!replaceCharacter.equals(other.replaceCharacter))
      return false;
    return true;
  }
}
