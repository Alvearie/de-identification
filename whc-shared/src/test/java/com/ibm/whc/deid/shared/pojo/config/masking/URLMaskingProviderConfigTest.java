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

public class URLMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    URLMaskingProviderConfig config = new URLMaskingProviderConfig();
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

    config.setPreserveDomains(-2);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`preserveDomains` must not be less than -1", e.getMessage());
    }
    config.setPreserveDomains(-1);
    config.validate();
    config.setPreserveDomains(0);
    config.validate();
    config.setPreserveDomains(1);
    config.validate();
    config.setPreserveDomains(10);
    config.validate();
  }

}
