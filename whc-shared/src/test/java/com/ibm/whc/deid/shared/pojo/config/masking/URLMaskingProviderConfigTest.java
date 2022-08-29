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

public class URLMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    URLMaskingProviderConfig config = new URLMaskingProviderConfig();
    config.validate(null);

    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    config.validate(null);

    config.setPreserveDomains(-2);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`preserveDomains` must not be less than -1", e.getMessage());
    }
    config.setPreserveDomains(-1);
    config.validate(null);
    config.setPreserveDomains(0);
    config.validate(null);
    config.setPreserveDomains(1);
    config.validate(null);
    config.setPreserveDomains(10);
    config.validate(null);
  }
}
