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
 * Replaces a marital status with a randomly chosen marital status.
 */
@JsonInclude(Include.NON_NULL)
public class MaritalStatusMaskingProviderConfig extends MaskingProviderConfig {
  private static final long serialVersionUID = -6920347830458699001L;

  public MaritalStatusMaskingProviderConfig() {
    type = MaskingProviderType.MARITAL;
  }
}
