/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.ReplaceMaskingProviderConfig;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class ReplaceMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    ReplaceMaskingProviderConfig config = new ReplaceMaskingProviderConfig();
    config.validate();
    config.setUnspecifiedValueHandling(4);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(3);
    config.validate();
    config.setMaskOffset(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`maskOffset` must be greater than or equal to 0"));
    }
    config.setMaskOffset(0);
    config.validate();
    config.setMaskPreserve(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`maskPreserve` must be greater than or equal to 0"));
    }
    config.setMaskPreserve(0);
    config.validate();
  }
}
