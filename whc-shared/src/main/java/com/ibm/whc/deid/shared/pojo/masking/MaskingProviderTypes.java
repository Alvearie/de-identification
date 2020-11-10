/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType.MaskingProviderCategory;

/*
 * Interface for MaskingProviderTypes - must use.
 */
public interface MaskingProviderTypes {
  
  public String getIdentifier();

  public default MaskingProviderCategory getCategory() {
    return MaskingProviderCategory.CategoryII;
  }
  
}
