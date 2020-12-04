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

public class EmailMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    EmailMaskingProviderConfig config = new EmailMaskingProviderConfig();
    config.validate();

    config.setUnspecifiedValueHandling(4);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(2);
    config.validate();

    config.setNameLength(-2);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`nameLength` must be -1 or greater than 0", e.getMessage());
    }

    config.setNameLength(-1);
    config.validate();

    config.setNameLength(0);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`nameLength` must be -1 or greater than 0", e.getMessage());
    }

    config.setNameLength(1);
    config.validate();
    config.setNameLength(5);
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
