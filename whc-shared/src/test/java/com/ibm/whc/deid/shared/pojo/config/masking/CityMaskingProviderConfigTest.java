/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import org.junit.Test;

public class CityMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    CityMaskingProviderConfig config = new CityMaskingProviderConfig();
    config.validate();

    config.setMaskClosestK(0);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`maskClosestK` must be greater than 0"));
    }

    config.setMaskClosestK(5);
    config.validate();

    config.setMaskClosestK(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`maskClosestK` must be greater than 0"));
    }
  }

}
