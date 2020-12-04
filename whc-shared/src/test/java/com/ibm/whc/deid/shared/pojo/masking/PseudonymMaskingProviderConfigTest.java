/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.PseudonymMaskingProviderConfig;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class PseudonymMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    PseudonymMaskingProviderConfig config = new PseudonymMaskingProviderConfig();
    config.validate();
    config.setGenerateViaPatternLanguageCode(null);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`generateViaPatternLanguageCode` must not be null"));
    }
    config.setGenerateViaPatternLanguageCode("EN");
    config.validate();
    config.setGenerateViaOptionsMinLength(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage()
          .contains("`generateViaOptionsMinLength` must be greater than or equal to 1"));
    }
    config.setGenerateViaOptionsMinLength(2);
    config.validate();
    config.setGenerateViaOptionsMaxLength(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage()
          .contains("`generateViaOptionsMaxLength` must be greater than or equal to 1"));
    }
    config.setGenerateViaOptionsMaxLength(1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage()
          .contains(
              "`generateViaOptionsMaxLength` must be greater than `generateViaOptionsMinLength`"));
    }
    config.setGenerateViaOptionsMaxLength(2);
    config.validate();
  }
}
