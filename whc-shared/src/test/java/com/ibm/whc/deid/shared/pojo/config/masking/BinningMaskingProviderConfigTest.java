/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class BinningMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    BinningMaskingProviderConfig config = new BinningMaskingProviderConfig();
    config.validate(null);

    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    config.validate(null);

    config.setBinSize(0);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`binSize` must be greater than 0", e.getMessage());
    }

    config.setBinSize(1);
    config.validate(null);

    config.setBinSize(1000000);
    config.validate(null);

    config.setBinSize(-1);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`binSize` must be greater than 0", e.getMessage());
    }
    config.setBinSize(5);

    config.validate(null);
    config.setUseSingleBucketOverThreshold(true);
    config.setUseSingleBucketUnderThreshold(true);
    config.setSingleBucketOverThresholdValue(50.0);
    config.setSingleBucketUnderThresholdValue(60.0);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains(
          "`singleBucketOverThresholdValue` must be greater than or equal to 'singleBucketUnderThresholdValue'"));
    }
  }

  @Test
  public void testSetFormat() {
    BinningMaskingProviderConfig config = new BinningMaskingProviderConfig();
    config.setFormat("abc");
    assertEquals("abc", config.getFormat());
    config.setFormat(null);
    assertEquals(BinningMaskingProviderConfig.DEFAULT_FORMAT, config.getFormat());
  }

}
