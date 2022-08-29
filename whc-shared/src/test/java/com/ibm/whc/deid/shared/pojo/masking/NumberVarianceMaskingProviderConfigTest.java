/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.NumberVarianceMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class NumberVarianceMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    NumberVarianceMaskingProviderConfig config = new NumberVarianceMaskingProviderConfig();
    config.validate(null);
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    config.validate(null);
    config.setAugmentLowerBound(-1.0);
    config.validate(null);
    config.setAugmentMask(true);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`augmentLowerBound` must be greater than 0"));
    }
    config.setAugmentLowerBound(2.0);
    config.validate(null);
    config.setAugmentMask(false);
    config.setAugmentUpperBound(-1.0);
    config.validate(null);
    config.setAugmentMask(true);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`augmentUpperBound` must be greater than 0"));
    }
    config.setResultWithPrecision(true);
    config.setAugmentUpperBound(1.0);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains(
          "`augmentLowerBound` must be less than or equal to `augmentUpperBound` when `resultWithPrecision` is true"));
    }
    config.setAugmentMask(false);
    config.validate(null);
    config.setAugmentMask(true);
    config.setAugmentUpperBound(2.0);
    config.validate(null);
    config.setResultWithPrecision(false);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains(
          "`augmentLowerBound` must be less than `augmentUpperBound` when `resultWithPrecision` is false"));
    }
    config.setAugmentMask(false);
    config.validate(null);
    config.setAugmentUpperBound(3.0);
    config.validate(null);
  }
}
