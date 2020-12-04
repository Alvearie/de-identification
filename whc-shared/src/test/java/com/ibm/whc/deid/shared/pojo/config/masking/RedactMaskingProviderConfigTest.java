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

public class RedactMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    RedactMaskingProviderConfig config = new RedactMaskingProviderConfig();
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

    config.setReplaceCharacter("");
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`replaceCharacter` must be a single character", e.getMessage());
    }

    config.setReplaceCharacter(" ");
    config.validate();
    config.setReplaceCharacter(".");
    config.validate();
    
    config.setReplaceCharacter("ab");
    try {
      config.validate();
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`replaceCharacter` must be a single character", e.getMessage());
    }

    config.setReplaceCharacter(null);  // uses default
    config.validate();
    assertEquals("X", config.getReplaceCharacter());
  }
  
}
