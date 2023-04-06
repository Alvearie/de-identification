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
 * Replaces race information with a randomly chosen race.
 */
@JsonInclude(Include.NON_NULL)
public class RaceEthnicityMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -610366575039558817L;

  public RaceEthnicityMaskingProviderConfig() {
    type = MaskingProviderType.RACE;
  }
}
