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
import com.ibm.whc.deid.shared.pojo.config.masking.NumberVarianceMaskingProviderConfig;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class NumberVarianceMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    NumberVarianceMaskingProviderConfig config = new NumberVarianceMaskingProviderConfig();
    config.validate();
    config.setUnspecifiedValueHandling(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(3);
    config.validate();    
    config.setAugmentLowerBound(-1.0);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`augmentLowerBound` must be greater than 0"));
    }
    config.setAugmentLowerBound(1.0);
    config.validate();
    config.setAugmentUpperBound(-1.0);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`augmentUpperBound` must be greater than 0"));
    }
    config.setAugmentUpperBound(0.0);
    config.validate();
    config.setAugmentMask(true);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains(
          "`augmentLowerBound` must be less than or equal to `augmentUpperBound` when `augmentMask` is true"));
    }
    config.setAugmentUpperBound(5.0);
    config.validate();
    config.setAugmentUpperBound(1.0);
    config.setResultWithPrecision(false);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains(
          "`augmentUpperBound` must not be equal to `augmentLowerBound` when `resultWithPrecision` is false"));
    }
    config.setResultWithPrecision(true);
    config.validate();
  }
}
