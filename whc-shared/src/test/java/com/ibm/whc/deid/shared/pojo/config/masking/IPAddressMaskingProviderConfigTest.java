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

public class IPAddressMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    IPAddressMaskingProviderConfig config = new IPAddressMaskingProviderConfig();
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

    config.setSubnetsPreserve(-1);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`subnetsPreserve` must be greater than or equal to 0", e.getMessage());
    }
    config.setSubnetsPreserve(0);
    config.validate();
    config.setSubnetsPreserve(1);
    config.validate();
    config.setSubnetsPreserve(1);
    config.validate();
  }

}
