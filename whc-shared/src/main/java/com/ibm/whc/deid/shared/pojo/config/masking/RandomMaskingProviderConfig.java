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
 * Replaces original data value with random characters
 */
@JsonInclude(Include.NON_NULL)
public class RandomMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -2284734602193939229L;

  public RandomMaskingProviderConfig() {
    type = MaskingProviderType.RANDOM;
  }
}
