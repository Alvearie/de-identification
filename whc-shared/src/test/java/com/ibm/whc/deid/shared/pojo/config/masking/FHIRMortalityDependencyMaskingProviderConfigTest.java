/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class FHIRMortalityDependencyMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    FHIRMortalityDependencyMaskingProviderConfig config =
        new FHIRMortalityDependencyMaskingProviderConfig();
    config.validate(null);

    config.setUnspecifiedValueHandling(-4);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(3);
    config.validate(null);
  }
}
