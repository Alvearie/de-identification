/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class CityMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    CityMaskingProviderConfig config = new CityMaskingProviderConfig();
    config.validate(null);

    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    config.validate(null);

    config.setMaskClosestK(0);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskClosestK` must be greater than 0", e.getMessage());
    }

    config.setMaskClosestK(5);
    config.validate(null);

    config.setMaskClosestK(-1);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskClosestK` must be greater than 0", e.getMessage());
    }
  }
}
