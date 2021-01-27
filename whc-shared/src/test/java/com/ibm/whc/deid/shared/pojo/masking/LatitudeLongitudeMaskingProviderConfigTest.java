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
import com.ibm.whc.deid.shared.pojo.config.masking.LatitudeLongitudeMaskingProviderConfig;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class LatitudeLongitudeMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    LatitudeLongitudeMaskingProviderConfig config = new LatitudeLongitudeMaskingProviderConfig();
    config.validate();
    config.setUnspecifiedValueHandling(4);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(2);
    config.validate();
    config.setOffsetMaximumRadius(10);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`offsetMaximumRadius` must be greater than 10"));
    }
    config.setOffsetMaximumRadius(11);
    config.validate();
    config.setOffsetMinimumRadius(10);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`offsetMinimumRadius` must be greater than 10"));
    }
    config.setOffsetMinimumRadius(11);
    config.validate();
  }
}
