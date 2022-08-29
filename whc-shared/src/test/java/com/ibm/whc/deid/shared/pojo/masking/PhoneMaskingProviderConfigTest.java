/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.PhoneMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class PhoneMaskingProviderConfigTest {

  @Test
  public void testValidate() throws Exception {
    PhoneMaskingProviderConfig config = new PhoneMaskingProviderConfig();
    config.validate(null);

    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    config.validate(null);

    config.setInvNdigitsReplaceWith(null);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage().contains("`invNdigitsReplaceWith` must be not null"));
    }
    config.setInvNdigitsReplaceWith("1");
    config.validate(null);

    List<String> patterns = new ArrayList<>();
    config.setPhoneRegexPatterns(patterns);
    patterns.add("[0-9");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(
          e.getMessage().startsWith("pattern at offset 0 in `phoneRegexPatterns` is not valid: "));
    }
    patterns.clear();
    patterns.add("^(?<prefix>\\+|00)(?<countryCode>\\d{1,3})(?<separator>-| )(?<number>\\d+)");
    patterns.add(
        "^(?<prefix>\\+|00)(?<countryCode>\\d{1,3})(?<separator>-| )(?<number>\\(\\d+\\))\\d+");
    config.validate(null);
    patterns.add(null);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("pattern at offset 2 in `phoneRegexPatterns` is empty", e.getMessage());
    }
    patterns.remove(2);
    patterns.add("  \t");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("pattern at offset 2 in `phoneRegexPatterns` is empty", e.getMessage());
    }
    patterns.remove(2);
    patterns.add("[0-9]*");
    config.validate(null);
  }
}
