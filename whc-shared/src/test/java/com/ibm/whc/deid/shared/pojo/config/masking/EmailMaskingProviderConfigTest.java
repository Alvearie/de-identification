/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class EmailMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    EmailMaskingProviderConfig config = new EmailMaskingProviderConfig();
    config.validate(null);

    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    config.validate(null);

    config.setNameLength(-2);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`nameLength` must be -1 or greater than 0", e.getMessage());
    }

    config.setNameLength(-1);
    config.validate(null);

    config.setNameLength(0);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`nameLength` must be -1 or greater than 0", e.getMessage());
    }

    config.setNameLength(1);
    config.validate(null);
    config.setNameLength(5);
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
