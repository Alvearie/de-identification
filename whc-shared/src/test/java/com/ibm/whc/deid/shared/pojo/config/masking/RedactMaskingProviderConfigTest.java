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

public class RedactMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    RedactMaskingProviderConfig config = new RedactMaskingProviderConfig();
    config.validate(null);

    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    config.validate(null);

    config.setReplaceCharacter("");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`replaceCharacter` must be a single character", e.getMessage());
    }

    config.setReplaceCharacter(" ");
    config.validate(null);
    config.setReplaceCharacter(".");
    config.validate(null);

    config.setReplaceCharacter("ab");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`replaceCharacter` must be a single character", e.getMessage());
    }

    config.setReplaceCharacter(null); // uses default
    config.validate(null);
    assertEquals("X", config.getReplaceCharacter());
  }
}
