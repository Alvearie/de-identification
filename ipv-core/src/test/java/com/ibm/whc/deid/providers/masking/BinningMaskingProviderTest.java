/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.BinningMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.MaskingProviderConfig;

public class BinningMaskingProviderTest extends TestLogSetUp {
  @Test
  public void testDefault() {
    BinningMaskingProviderConfig config = new BinningMaskingProviderConfig();
    MaskingProvider maskingProvider = new BinningMaskingProvider(config);

    assertEquals("0-5", maskingProvider.mask("0"));
    assertEquals("0-5", maskingProvider.mask("0.1"));
    assertEquals("0-5", maskingProvider.mask("3"));
    assertEquals("0-5", maskingProvider.mask("3.01"));
    assertEquals("0-5", maskingProvider.mask("4.99"));
    assertEquals("5-10", maskingProvider.mask("5"));
    assertEquals("5-10", maskingProvider.mask("5.0"));
    assertEquals("5-10", maskingProvider.mask("5.000001"));
    assertEquals("5-10", maskingProvider.mask("9.9999999"));
    assertEquals("20-25", maskingProvider.mask("21.34"));
    assertEquals("20-25", maskingProvider.mask("22"));
    assertEquals("-5-0", maskingProvider.mask("-0.01"));
    assertEquals("-5-0", maskingProvider.mask("-4.9"));
    assertEquals("-5-0", maskingProvider.mask("-5.00000"));
    assertEquals("-10--5", maskingProvider.mask("-5.01"));
    assertEquals("-10--5", maskingProvider.mask("-8"));
    assertEquals("-10--5", maskingProvider.mask("-9.98"));
    assertEquals("-10--5", maskingProvider.mask("-10.00"));
    assertEquals("-10--5", maskingProvider.mask("-10"));
    assertEquals("-15--10", maskingProvider.mask("-10.1"));

  }

  @Test
  public void testInvalidValue() {
    BinningMaskingProviderConfig config = new BinningMaskingProviderConfig();
    config.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider = new BinningMaskingProvider(config);

    String originalValue = "abc";
    String maskedValue = maskingProvider.mask(originalValue);
    assertNull(maskedValue);

    config = new BinningMaskingProviderConfig();
    config.setUnspecifiedValueHandling(2);
    maskingProvider = new BinningMaskingProvider(config);

    maskedValue = maskingProvider.mask(originalValue);
    assertNull(maskedValue);

    config = new BinningMaskingProviderConfig();
    config.setUnspecifiedValueHandling(3);
    maskingProvider = new BinningMaskingProvider(config);

    maskedValue = maskingProvider.mask(originalValue);
    assertEquals(MaskingProviderConfig.UNSPECIFIED_VALUE_RETURN_MESSAGE_OTHER, maskedValue);

    config = new BinningMaskingProviderConfig();
    config.setUnspecifiedValueHandling(3);
    config.setUnspecifiedValueReturnMessage("message");
    maskingProvider = new BinningMaskingProvider(config);

    maskedValue = maskingProvider.mask(originalValue);
    assertEquals("message", maskedValue);
  }

  @Test
  public void testFormat() {
    BinningMaskingProviderConfig config = new BinningMaskingProviderConfig();
    config.setFormat("[%s - %s]");
    MaskingProvider maskingProvider = new BinningMaskingProvider(config);
    assertEquals("[20 - 25]", maskingProvider.mask("22"));
  }

  @Test
  public void testStartValue() {
    BinningMaskingProviderConfig config = new BinningMaskingProviderConfig();
    config.setStartValue(2);
    config.setBinSize(10);
    config.setUseStartValue(true);
    config.setFormat("%s:%s");
    MaskingProvider maskingProvider = new BinningMaskingProvider(config);

    assertEquals("-48:-38", maskingProvider.mask("-40"));
    assertEquals("-18:-8", maskingProvider.mask("-8.1"));
    assertEquals("-18:-8", maskingProvider.mask("-8.000001"));
    assertEquals("-8:2", maskingProvider.mask("-8"));
    assertEquals("-8:2", maskingProvider.mask("-8.0000000"));
    assertEquals("-8:2", maskingProvider.mask("-7.99999"));
    assertEquals("-8:2", maskingProvider.mask("0"));
    assertEquals("-8:2", maskingProvider.mask("1.999"));
    assertEquals("2:12", maskingProvider.mask("2"));
    assertEquals("2:12", maskingProvider.mask("5"));
    assertEquals("2:12", maskingProvider.mask("11.9"));
    assertEquals("12:22", maskingProvider.mask("12"));
    assertEquals("12:22", maskingProvider.mask("12.0"));
    assertEquals("12:22", maskingProvider.mask("12.01"));
    assertEquals("12:22", maskingProvider.mask("15"));
    assertEquals("7999999992:8000000002", maskingProvider.mask("8000000000"));
  }

  @Test
  public void testStartValueGreaterThanBinSize() {
    BinningMaskingProviderConfig config = new BinningMaskingProviderConfig();
    config.setStartValue(1002);
    config.setUseStartValue(true);
    config.setFormat("%s:%s");
    config.setBinSize(10);
    MaskingProvider maskingProvider = new BinningMaskingProvider(config);

    assertEquals("-8000000008:-7999999998", maskingProvider.mask("-8000000000"));
    assertEquals("-48:-38", maskingProvider.mask("-40"));
    assertEquals("-18:-8", maskingProvider.mask("-8.1"));
    assertEquals("-18:-8", maskingProvider.mask("-8.000001"));
    assertEquals("-8:2", maskingProvider.mask("-8"));
    assertEquals("-8:2", maskingProvider.mask("-8.0000000"));
    assertEquals("-8:2", maskingProvider.mask("-7.99999"));
    assertEquals("-8:2", maskingProvider.mask("0"));
    assertEquals("-8:2", maskingProvider.mask("1.999"));
    assertEquals("2:12", maskingProvider.mask("2"));
    assertEquals("2:12", maskingProvider.mask("5"));
    assertEquals("2:12", maskingProvider.mask("11.9"));
    assertEquals("12:22", maskingProvider.mask("12"));
    assertEquals("12:22", maskingProvider.mask("12.0"));
    assertEquals("12:22", maskingProvider.mask("12.01"));
    assertEquals("12:22", maskingProvider.mask("15"));
    assertEquals("7999999992:8000000002", maskingProvider.mask("8000000000"));
  }

  @Test
  public void testStartValueNegative() {
    BinningMaskingProviderConfig config = new BinningMaskingProviderConfig();
    config.setStartValue(-17);
    config.setUseStartValue(true);
    config.setFormat("%s:%s");
    config.setBinSize(6);
    MaskingProvider maskingProvider = new BinningMaskingProvider(config);

    assertEquals("-53:-47", maskingProvider.mask("-48"));
    assertEquals("-11:-5", maskingProvider.mask("-8.1"));
    assertEquals("-17:-11", maskingProvider.mask("-11.0001"));
    assertEquals("-11:-5", maskingProvider.mask("-11.000"));
    assertEquals("-11:-5", maskingProvider.mask("-11"));
    assertEquals("-11:-5", maskingProvider.mask("-10.99999"));
    assertEquals("-11:-5", maskingProvider.mask("-8"));
    assertEquals("-11:-5", maskingProvider.mask("-5.0000001"));
    assertEquals("-5:1", maskingProvider.mask("-.0000001"));
    assertEquals("-5:1", maskingProvider.mask("0"));
    assertEquals("1:7", maskingProvider.mask("1.999"));
    assertEquals("1:7", maskingProvider.mask("2"));
    assertEquals("7:13", maskingProvider.mask("7"));
    assertEquals("7:13", maskingProvider.mask("7.0001"));
  }

  @Test
  public void testConstantValues() {
    BinningMaskingProviderConfig config = new BinningMaskingProviderConfig();
    config.setUseSingleBucketOverThreshold(true);
    config.setUseSingleBucketUnderThreshold(true);
    MaskingProvider maskingProvider = new BinningMaskingProvider(config);

    assertEquals("<10", maskingProvider.mask("0"));
    assertEquals("<10", maskingProvider.mask("0.1"));
    assertEquals("<10", maskingProvider.mask("3"));
    assertEquals("<10", maskingProvider.mask("3.01"));
    assertEquals("<10", maskingProvider.mask("5.0"));
    assertEquals("<10", maskingProvider.mask("5.000001"));
    assertEquals("<10", maskingProvider.mask("9.9999999"));
    assertEquals("<10", maskingProvider.mask("-0.01"));
    assertEquals("<10", maskingProvider.mask("-4.9"));
    assertEquals("<10", maskingProvider.mask("-7.3"));
    assertEquals("<10", maskingProvider.mask("-100.5"));
    assertEquals("10-15", maskingProvider.mask("10"));
    assertEquals("50-55", maskingProvider.mask("50.2"));
    assertEquals("85-90", maskingProvider.mask("89.9999"));
    assertEquals("90+", maskingProvider.mask("90"));
    assertEquals("90+", maskingProvider.mask("92.2"));
    assertEquals("90+", maskingProvider.mask("120"));


    // changing default values
    config.setSingleBucketOverThresholdValue(110);
    config.setSingleBucketUnderThresholdValue(5);
    config.setSingleBucketOverThresholdReplacement("110+");
    config.setSingleBucketUnderThresholdReplacement("<5");
    maskingProvider = new BinningMaskingProvider(config);
    assertEquals("<5", maskingProvider.mask("0"));
    assertEquals("<5", maskingProvider.mask("0.1"));
    assertEquals("<5", maskingProvider.mask("3"));
    assertEquals("<5", maskingProvider.mask("3.01"));
    assertEquals("<5", maskingProvider.mask("4.9999999"));
    assertEquals("<5", maskingProvider.mask("-4.9"));
    assertEquals("5-10", maskingProvider.mask("5.0"));
    assertEquals("<5", maskingProvider.mask("-7.3"));
    assertEquals("50-55", maskingProvider.mask("50.2"));
    assertEquals("105-110", maskingProvider.mask("109.9999"));
    assertEquals("110+", maskingProvider.mask("110"));
    assertEquals("110+", maskingProvider.mask("112.2"));
    assertEquals("110+", maskingProvider.mask("120"));
    assertEquals("<5", maskingProvider.mask("-115.5"));

  }

  @Test
  public void testUpperThresholdValues() {
    BinningMaskingProviderConfig config = new BinningMaskingProviderConfig();
    config.setUseSingleBucketOverThreshold(true);
    MaskingProvider maskingProvider = new BinningMaskingProvider(config);

    assertEquals("50-55", maskingProvider.mask("50.2"));
    assertEquals("85-90", maskingProvider.mask("89.9999"));
    assertEquals("90+", maskingProvider.mask("90"));
    assertEquals("90+", maskingProvider.mask("92.2"));
    assertEquals("90+", maskingProvider.mask("120"));

    // changing default values
    config.setSingleBucketOverThresholdValue(110);
    config.setSingleBucketOverThresholdReplacement("110+");
    maskingProvider = new BinningMaskingProvider(config);
    assertEquals("50-55", maskingProvider.mask("50.2"));
    assertEquals("105-110", maskingProvider.mask("109.9999"));
    assertEquals("110+", maskingProvider.mask("110"));
    assertEquals("110+", maskingProvider.mask("112.2"));
    assertEquals("110+", maskingProvider.mask("120"));

  }

  @Test
  public void testLowerThresholdValues() {
    BinningMaskingProviderConfig config = new BinningMaskingProviderConfig();
    config.setUseSingleBucketUnderThreshold(true);
    MaskingProvider maskingProvider = new BinningMaskingProvider(config);

    assertEquals("<10", maskingProvider.mask("0"));
    assertEquals("<10", maskingProvider.mask("0.1"));
    assertEquals("<10", maskingProvider.mask("3"));
    assertEquals("<10", maskingProvider.mask("3.01"));
    assertEquals("<10", maskingProvider.mask("5.0"));
    assertEquals("<10", maskingProvider.mask("5.000001"));
    assertEquals("<10", maskingProvider.mask("9.9999999"));
    assertEquals("<10", maskingProvider.mask("-0.01"));
    assertEquals("<10", maskingProvider.mask("-4.9"));
    assertEquals("<10", maskingProvider.mask("-7.3"));
    assertEquals("<10", maskingProvider.mask("-100.5"));
    assertEquals("10-15", maskingProvider.mask("10"));

    // changing default values
    config.setSingleBucketUnderThresholdValue(5);
    config.setSingleBucketUnderThresholdReplacement("<5");
    maskingProvider = new BinningMaskingProvider(config);
    assertEquals("<5", maskingProvider.mask("0"));
    assertEquals("<5", maskingProvider.mask("0.1"));
    assertEquals("<5", maskingProvider.mask("3"));
    assertEquals("<5", maskingProvider.mask("3.01"));
    assertEquals("<5", maskingProvider.mask("4.9999999"));
    assertEquals("<5", maskingProvider.mask("-4.9"));
    assertEquals("5-10", maskingProvider.mask("5.0"));
    assertEquals("<5", maskingProvider.mask("-7.3"));
    assertEquals("50-55", maskingProvider.mask("50.2"));
    assertEquals("90-95", maskingProvider.mask("90.2"));

  }
}
