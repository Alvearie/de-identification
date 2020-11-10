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
 * Masks SWIFT codes with the option to preserve their country.
 */
@JsonInclude(Include.NON_NULL)
public class SWIFTMaskingProviderConfig extends MaskingProviderConfig {
  private static final long serialVersionUID = -1616246339064360568L;

  boolean preserveCountry = false;

  public SWIFTMaskingProviderConfig() {
    type = MaskingProviderType.SWIFT;
  }

  public boolean isPreserveCountry() {
    return preserveCountry;
  }

  public void setPreserveCountry(boolean preserveCountry) {
    this.preserveCountry = preserveCountry;
  }
}
