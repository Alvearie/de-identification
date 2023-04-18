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
 * Retains the current value of the field.
 */
@JsonInclude(Include.NON_NULL)
public class MaintainMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -4334059186680999571L;

  public MaintainMaskingProviderConfig() {
    type = MaskingProviderType.MAINTAIN;
  }
}
