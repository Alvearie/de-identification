/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import org.junit.Test;

public class ATCMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    ATCMaskingProviderConfig config = new ATCMaskingProviderConfig();
    config.validate();

    config.setMaskLevelsToKeep(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`maskLevelsToKeep`"));
    }

    config.setMaskLevelsToKeep(0);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`maskLevelsToKeep`"));
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
      assertTrue(e.getMessage().contains("`maskLevelsToKeep`"));
    }

    config.setMaskLevelsToKeep(1000000);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`maskLevelsToKeep`"));
    }
  }

}
