/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.LatitudeLongitudeMaskingProviderConfig;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class LatitudeLongitudeMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    LatitudeLongitudeMaskingProviderConfig config = new LatitudeLongitudeMaskingProviderConfig();
    config.validate();
    config.setOffsetMaximumRadius(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`offsetMaximumRadius` must be greater than 0"));
    }
    config.setOffsetMaximumRadius(0);
    config.validate();
    config.setOffsetMinimumRadius(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`offsetMinimumRadius` must be greater than 0"));
    }
    config.setOffsetMinimumRadius(0);
    config.validate();
  }
}
