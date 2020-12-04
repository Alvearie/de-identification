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
import com.ibm.whc.deid.shared.pojo.config.masking.PhoneMaskingProviderConfig;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class PhoneMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    PhoneMaskingProviderConfig config = new PhoneMaskingProviderConfig();
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
    config.setInvNdigitsReplaceWith(null);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`invNdigitsReplaceWith` must be not null"));
    }
    config.setInvNdigitsReplaceWith("1");
    config.validate();
  }
}
