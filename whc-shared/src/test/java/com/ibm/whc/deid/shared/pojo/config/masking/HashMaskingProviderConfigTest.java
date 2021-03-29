/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import org.junit.Test;

public class HashMaskingProviderConfigTest {

  @Test
  public void testSetSalt() {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    assertEquals("", config.getSalt());
    config.setSalt("xx");
    assertEquals("xx", config.getSalt());
    config.setSalt(null);
    assertEquals("", config.getSalt());
  }

  @Test
  public void testValidate() throws Exception {
    HashMaskingProviderConfig config = new HashMaskingProviderConfig();
    config.validate(null);

    config.setUnspecifiedValueHandling(5);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`unspecifiedValueHandling` must be [0..3]", e.getMessage());
    }
    config.setUnspecifiedValueHandling(3);
    config.validate(null);

    config.setAlgorithmDefault(null);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`algorithmDefault` is required", e.getMessage());
    }
    config.setAlgorithmDefault(" ");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`algorithmDefault` is required", e.getMessage());
    }
    // an algorithm not available on this system, might be available on the target system, so allow
    config.setAlgorithmDefault("myAlgorithm");
    config.validate(null);
  }
}
