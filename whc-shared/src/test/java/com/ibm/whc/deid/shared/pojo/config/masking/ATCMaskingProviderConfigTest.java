/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class ATCMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    ATCMaskingProviderConfig config = new ATCMaskingProviderConfig();
    config.validate(null);

    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    config.validate(null);
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    config.validate(null);
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    config.validate(null);

    config.setMaskLevelsToKeep(-1);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskLevelsToKeep` must be [1..4]", e.getMessage());
    }

    config.setMaskLevelsToKeep(0);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskLevelsToKeep` must be [1..4]", e.getMessage());
    }

    config.setMaskLevelsToKeep(1);
    config.validate(null);

    config.setMaskLevelsToKeep(2);
    config.validate(null);

    config.setMaskLevelsToKeep(3);
    config.validate(null);

    config.setMaskLevelsToKeep(4);
    config.validate(null);

    config.setMaskLevelsToKeep(5);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskLevelsToKeep` must be [1..4]", e.getMessage());
    }

    config.setMaskLevelsToKeep(1000000);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`maskLevelsToKeep` must be [1..4]", e.getMessage());
    }
  }
}
