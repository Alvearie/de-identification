/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.ZIPCodeMaskingProviderConfig;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class ZIPCodeMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    ZIPCodeMaskingProviderConfig config = new ZIPCodeMaskingProviderConfig();
    config.validate(null);
    config.setUnspecifiedValueHandling(-2);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(3);
    config.validate(null);
    config.setMaskCountryCode(null);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`maskCountryCode` must not be null"));
    }
    config.setMaskCountryCode("US");
    config.validate(null);
    config.setMaskReplaceWithNeighborNearestCount(-1);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(
          e.getMessage().contains("`maskReplaceWithNeighborNearestCount` must be greater than 0"));
    }
    config.setMaskReplaceWithNeighborNearestCount(1);
    config.validate(null);
    config.setMaskPrefixLength(-1);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`maskPrefixLength` must be greater than 0"));
    }
    config.setMaskPrefixLength(1);
    config.validate(null);
    config.setMaskPrefixMinPopulation(-1);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`maskPrefixMinPopulation` must be greater than 0"));
    }
    config.setMaskPrefixMinPopulation(1);
    config.validate(null);
    config.setMaskTruncateLengthIfNotMinPopulation(-1);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(
          e.getMessage().contains("`maskTruncateLengthIfNotMinPopulation` must be greater than 0"));
    }
    config.setMaskTruncateLengthIfNotMinPopulation(1);
    config.validate(null);
  }
}
