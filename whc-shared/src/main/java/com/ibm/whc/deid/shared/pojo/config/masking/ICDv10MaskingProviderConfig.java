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
 * Masks ICDV10 (diagnoses) codes
 */
@JsonInclude(Include.NON_NULL)
public class ICDv10MaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 8046704785024566040L;
  boolean randomizeChapter = false;
  boolean randomizeCategory = true;

  public ICDv10MaskingProviderConfig() {
    type = MaskingProviderType.ICDV10;
  }

  public boolean isRandomizeChapter() {
    return randomizeChapter;
  }

  public void setRandomizeChapter(boolean randomizeChapter) {
    this.randomizeChapter = randomizeChapter;
  }

  public boolean isRandomizeCategory() {
    return randomizeCategory;
  }

  public void setRandomizeCategory(boolean randomizeCategory) {
    this.randomizeCategory = randomizeCategory;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (randomizeCategory ? 1231 : 1237);
    result = prime * result + (randomizeChapter ? 1231 : 1237);
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
    ICDv10MaskingProviderConfig other = (ICDv10MaskingProviderConfig) obj;
    if (randomizeCategory != other.randomizeCategory)
      return false;
    if (randomizeChapter != other.randomizeChapter)
      return false;
    return true;
  }
}
