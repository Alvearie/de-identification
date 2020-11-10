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
 * Replaces a GUID with a randomly chosen GUID.
 */
@JsonInclude(Include.NON_NULL)
public class GUIDMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 2831826937694587400L;

  public GUIDMaskingProviderConfig() {
    type = MaskingProviderType.GUID;
  }
}
