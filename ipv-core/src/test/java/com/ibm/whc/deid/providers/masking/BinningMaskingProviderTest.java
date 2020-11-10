/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;

import com.ibm.whc.deid.shared.pojo.config.masking.BinningMaskingProviderConfig;
import org.junit.Test;

public class BinningMaskingProviderTest extends TestLogSetUp {
  @Test
  public void testDefault() {
    BinningMaskingProviderConfig config = new BinningMaskingProviderConfig();
    MaskingProvider maskingProvider = new BinningMaskingProvider(config);

    String originalValue = "22";
    String maskedValue = maskingProvider.mask(originalValue);

    assertEquals("20-25", maskedValue);
  }

  @Test
  public void testDouble() {
    BinningMaskingProviderConfig config = new BinningMaskingProviderConfig();
    MaskingProvider maskingProvider = new BinningMaskingProvider(config);

    String originalValue = "21.34";
    String maskedValue = maskingProvider.mask(originalValue);

    assertEquals("20-25", maskedValue);
  }

  @Test(expected = NumberFormatException.class)
  public void testInvalidValue() {
    BinningMaskingProviderConfig config = new BinningMaskingProviderConfig();
    MaskingProvider maskingProvider = new BinningMaskingProvider(config);

    String originalValue = "abc";
    String maskedValue = maskingProvider.mask(originalValue);

    assertEquals(null, maskedValue);
  }

  @Test
  public void testFormat() {
    BinningMaskingProviderConfig config = new BinningMaskingProviderConfig();
    config.setFormat("[%s-%s]");
    MaskingProvider maskingProvider = new BinningMaskingProvider(config);

    String originalValue = "22";
    String maskedValue = maskingProvider.mask(originalValue);

    assertEquals("[20-25]", maskedValue);
  }

  @Test
  public void testStartValue() {
    BinningMaskingProviderConfig config = new BinningMaskingProviderConfig();
    config.setStartValue(7);
    config.setUseStartValue(Boolean.TRUE);
    MaskingProvider maskingProvider = new BinningMaskingProvider(config);

    String originalValue = "22";
    String maskedValue = maskingProvider.mask(originalValue);

    assertEquals("7-12", maskedValue);
  }
}
