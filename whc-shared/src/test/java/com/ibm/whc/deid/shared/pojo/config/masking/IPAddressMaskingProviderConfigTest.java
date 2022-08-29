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

public class IPAddressMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    IPAddressMaskingProviderConfig config = new IPAddressMaskingProviderConfig();
    config.validate(null);

    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    config.validate(null);

    config.setSubnetsPreserve(-1);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`subnetsPreserve` must be greater than or equal to 0", e.getMessage());
    }
    config.setSubnetsPreserve(0);
    config.validate(null);
    config.setSubnetsPreserve(1);
    config.validate(null);
    config.setSubnetsPreserve(1);
    config.validate(null);
  }

}
