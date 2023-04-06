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
 * Replaces a gender information with a randomly chosen gender.
 */
@JsonInclude(Include.NON_NULL)
public class GenderMaskingProviderConfig extends MaskingProviderConfig {
  private static final long serialVersionUID = 6458817236319246544L;

  public GenderMaskingProviderConfig() {
    type = MaskingProviderType.GENDER;
  }
}
