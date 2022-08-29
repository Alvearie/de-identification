/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.ReplaceMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class ReplaceMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    ReplaceMaskingProviderConfig config = new ReplaceMaskingProviderConfig();
    config.validate(null);
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    config.validate(null);
    config.setMaskOffset(-1);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`maskOffset` must be greater than or equal to 0"));
    }
    config.setMaskOffset(0);
    config.validate(null);
    config.setMaskPreserve(-1);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`maskPreserve` must be greater than or equal to 0"));
    }
    config.setMaskPreserve(0);
    config.validate(null);
  }
}
