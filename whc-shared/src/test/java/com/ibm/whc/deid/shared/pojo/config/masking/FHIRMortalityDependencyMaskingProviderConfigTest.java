/*
 * © Merative US L.P. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import org.junit.Test;

public class FHIRMortalityDependencyMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    FHIRMortalityDependencyMaskingProviderConfig config =
        new FHIRMortalityDependencyMaskingProviderConfig();
    config.validate(null);

    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    config.validate(null);
  }
}
