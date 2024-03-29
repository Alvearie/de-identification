/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;

/*
 * Masks ICDV9 (diagnoses) codes
 */
@JsonInclude(Include.NON_NULL)
public class ICDv9MaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -6357595760111730615L;
  boolean generalizeToChapter = false;
  boolean generalizeToCategory = true;

  public ICDv9MaskingProviderConfig() {
    type = MaskingProviderType.ICDV9;
  }

  public boolean isGeneralizeToChapter() {
    return generalizeToChapter;
  }

  public void setGeneralizeToChapter(boolean generalizeChapter) {
    this.generalizeToChapter = generalizeChapter;
  }

  public boolean isGeneralizeToCategory() {
    return generalizeToCategory;
  }

  public void setGeneralizeToCategory(boolean generalizeCategory) {
    this.generalizeToCategory = generalizeCategory;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (generalizeToCategory ? 1231 : 1237);
    result = prime * result + (generalizeToChapter ? 1231 : 1237);
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
    ICDv9MaskingProviderConfig other = (ICDv9MaskingProviderConfig) obj;
    if (generalizeToCategory != other.generalizeToCategory)
      return false;
    if (generalizeToChapter != other.generalizeToChapter)
      return false;
    return true;
  }
}
