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
 * Replaces religion information with a randomly chosen religion.
 */
@JsonInclude(Include.NON_NULL)
public class ReligionMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 8076978451552621715L;

  public ReligionMaskingProviderConfig() {
    type = MaskingProviderType.RELIGION;
  }
}
