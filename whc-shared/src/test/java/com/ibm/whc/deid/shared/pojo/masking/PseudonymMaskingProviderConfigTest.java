/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.PseudonymMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class PseudonymMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    PseudonymMaskingProviderConfig config = new PseudonymMaskingProviderConfig();
    config.validate(null);
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    config.validate(null);
    config.setGenerateViaPatternLanguageCode(null);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`generateViaPatternLanguageCode` must not be null"));
    }
    config.setGenerateViaPatternLanguageCode("EN");
    config.validate(null);
    config.setGenerateViaOptionsMinLength(0);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage()
          .contains("`generateViaOptionsMinLength` must be greater than or equal to 1"));
    }
    config.setGenerateViaOptionsMinLength(2);
    config.validate(null);
    config.setGenerateViaOptionsMaxLength(0);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage()
          .contains("`generateViaOptionsMaxLength` must be greater than or equal to 1"));
    }
    config.setGenerateViaOptionsMaxLength(1);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains(
          "`generateViaOptionsMaxLength` must be greater than or equal to `generateViaOptionsMinLength`"));
    }
    config.setGenerateViaOptionsMaxLength(2);
    config.validate(null);
    config.setGenerateViaOptionsMaxLength(3);
    config.validate(null);
  }
}
