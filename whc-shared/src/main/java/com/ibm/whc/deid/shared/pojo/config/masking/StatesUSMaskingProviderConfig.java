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
 * Replaces a state with a randomly chosen state.
 */
@JsonInclude(Include.NON_NULL)
public class StatesUSMaskingProviderConfig extends MaskingProviderConfig {
  private static final long serialVersionUID = -3048774218513702848L;

  public StatesUSMaskingProviderConfig() {
    type = MaskingProviderType.STATE_US;
  }
}
