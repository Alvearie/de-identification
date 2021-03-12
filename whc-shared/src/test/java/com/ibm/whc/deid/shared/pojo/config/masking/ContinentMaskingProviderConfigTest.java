/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import org.junit.Test;

public class ContinentMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    ContinentMaskingProviderConfig config = new ContinentMaskingProviderConfig();
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

    config.setMaskClosestK(0);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals(e.getMessage(), "`maskClosestK` must be greater than 0");
    }

    config.setMaskClosestK(5);
    config.validate(null);

    config.setMaskClosestK(-1);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals(e.getMessage(), "`maskClosestK` must be greater than 0");
    }
  }
}
