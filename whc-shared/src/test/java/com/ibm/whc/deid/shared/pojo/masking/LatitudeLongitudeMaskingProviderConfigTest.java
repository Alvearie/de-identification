/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.LatitudeLongitudeMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class LatitudeLongitudeMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    LatitudeLongitudeMaskingProviderConfig config = new LatitudeLongitudeMaskingProviderConfig();
    config.validate(null);

    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    config.validate(null);
    config.setOffsetMaximumRadius(10);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`offsetMaximumRadius` must be greater than 10"));
    }
    config.setOffsetMaximumRadius(11);
    config.validate(null);
    config.setOffsetMinimumRadius(10);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`offsetMinimumRadius` must be greater than 10"));
    }
    config.setOffsetMinimumRadius(11);
    config.validate(null);
  }
}
