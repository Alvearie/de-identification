/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.util.List;
import com.ibm.whc.deid.providers.masking.fhir.MaskingActionInputIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.MaintainMaskingProviderConfig;

public class MaintainMaskingProvider extends AbstractMaskingProvider {


  private static final long serialVersionUID = -1799896601470556794L;

  public MaintainMaskingProvider() {
    super(new MaintainMaskingProviderConfig());
  }

  @Override
  public void maskIdentifierBatch(List<MaskingActionInputIdentifier> identifiers) {
    // override superclass to retain data type of original node
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    return identifier;
  }
}
