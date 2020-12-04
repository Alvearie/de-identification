/*
 * (C) Copyright IBM Corp. 2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import org.junit.Test;

public class ATCMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    ATCMaskingProviderConfig config = new ATCMaskingProviderConfig();
    config.validate();

    config.setUnspecifiedValueHandling(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(0);
    config.validate();
    config.setUnspecifiedValueHandling(1);
    config.validate();
    config.setUnspecifiedValueHandling(2);
    config.validate();
    config.setUnspecifiedValueHandling(3);
    config.validate();
    config.setUnspecifiedValueHandling(4);    
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(0);

    config.setMaskLevelsToKeep(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskLevelsToKeep` must be [1..4]", e.getMessage());
    }

    config.setMaskLevelsToKeep(0);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskLevelsToKeep` must be [1..4]", e.getMessage());
    }

    config.setMaskLevelsToKeep(1);
    config.validate();

    config.setMaskLevelsToKeep(2);
    config.validate();

    config.setMaskLevelsToKeep(3);
    config.validate();

    config.setMaskLevelsToKeep(4);
    config.validate();

    config.setMaskLevelsToKeep(5);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskLevelsToKeep` must be [1..4]", e.getMessage());
    }

    config.setMaskLevelsToKeep(1000000);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskLevelsToKeep` must be [1..4]", e.getMessage());
    }
  }

}
