/*
 * (C) Copyright IBM Corp. 2016,2020
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

    config.setNameLength(-2);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals(e.getMessage(), "`nameLength` must be -1 or greater than 0");
    }

    config.setNameLength(-1);
    config.validate();

    config.setNameLength(0);
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals(e.getMessage(), "`nameLength` must be -1 or greater than 0");
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
      assertEquals(e.getMessage(), "`preserveDomains` must not be less than -1");
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
