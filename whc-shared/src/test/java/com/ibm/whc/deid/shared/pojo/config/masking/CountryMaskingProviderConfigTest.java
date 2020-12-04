/*
 * (C) Copyright IBM Corp. 2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import org.junit.Test;

public class CountryMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    CountryMaskingProviderConfig config = new CountryMaskingProviderConfig();
    config.validate();

    config.setUnspecifiedValueHandling(-4);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(3);
    config.validate();

    config.setMaskClosestK(0);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskClosestK` must be greater than 0", e.getMessage());
    }

    config.setMaskClosestK(5);
    config.validate();

    config.setMaskClosestK(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskClosestK` must be greater than 0", e.getMessage());
    }
  }

}
