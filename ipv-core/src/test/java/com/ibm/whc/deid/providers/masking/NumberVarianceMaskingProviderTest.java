/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.shared.pojo.config.masking.NumberVarianceMaskingProviderConfig;
import org.junit.Test;

public class NumberVarianceMaskingProviderTest extends TestLogSetUp {

  /*
   * Note: In addition to default range values, also tests for 1) when limitUp is zero, 2) when
   * limitDown is zero, and 3) when both limit up and down are zero (considered invalid and
   * exception thrown)
   */
  @Test
  public void testMask() {
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider();

    Double originalValue = new Double(50);
    String value = originalValue.toString();

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      Double maskedDouble = Double.valueOf(maskedValue);
      assertTrue(maskedDouble > 40.0);
      assertTrue(maskedDouble < 60.0);
    }
  }

  @Test
  public void testZero() {
    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setMaskLimitUp(0);
    configuration.setMaskLimitDown(0);

    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    Double originalValue = new Double(50);
    String value = originalValue.toString();

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      assertEquals(value, maskedValue);
    }
  }

  @Test
  public void testLimitUpZero_LimitDownInteger() {
    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setMaskLimitUp(0);
    configuration.setMaskLimitDown(35);

    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    Double originalValue = new Double(50);
    String value = originalValue.toString();

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      Double maskedDouble = Double.valueOf(maskedValue);
      assertTrue(maskedDouble >= 32.5);
      assertTrue(maskedDouble <= 50.0);
      // System.out.println("=======> LimitUp Zero, masked value [" +
      // maskedValue + "]");
    }
  }

  @Test
  public void testLimitUpZero_LimitDownDouble() {
    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setMaskLimitUp(0);
    configuration.setMaskLimitDown(35.5);
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    Double originalValue = new Double(50);
    String value = originalValue.toString();

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      Double maskedDouble = Double.valueOf(maskedValue);
      assertTrue(maskedDouble >= 32.5);
      assertTrue(maskedDouble <= 50.0);
      // System.out.println("=======> LimitUp Zero, masked value [" +
      // maskedValue + "]");
    }
  }

  @Test
  public void testLimitDownZero_LimitUpInteger() {
    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setMaskLimitUp(25);
    configuration.setMaskLimitDown(0);
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    Double originalValue = new Double(50);
    String value = originalValue.toString();

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      Double maskedDouble = Double.valueOf(maskedValue);
      assertTrue(maskedDouble >= 49.0);
      assertTrue(maskedDouble <= 62.5);
      // System.out.println("=======> LimitDwon Zero, masked value [" +
      // maskedValue + "]");
    }
  }

  @Test
  public void testLimitDownZero_LimitUpDouble() {
    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setMaskLimitUp(25.5);
    configuration.setMaskLimitDown(0);
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    Double originalValue = new Double(50);
    String value = originalValue.toString();

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      Double maskedDouble = Double.valueOf(maskedValue);
      assertTrue(maskedDouble >= 49.0);
      assertTrue(maskedDouble <= 62.5);
      // System.out.println("=======> LimitDwon Zero, masked value [" +
      // maskedValue + "]");
    }
  }

  @Test
  public void testAugmentWithNoPrecision_IntegerInput() {
    Double originalValue = new Double(50);
    String value = originalValue.toString();
    int lowerBound = 2;
    int upperBound = 4;

    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setAugmentMask(true);
    configuration.setAugmentLowerBound(lowerBound);
    configuration.setAugmentUpperBound(upperBound);
    configuration.setResultWithPrecision(false);
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      Double maskedDouble = Double.valueOf(maskedValue);
      assertTrue(maskedDouble >= originalValue);
      assertTrue(maskedDouble == (int) maskedDouble.doubleValue());
      assertTrue(maskedDouble <= originalValue + upperBound);
    }
  }

  @Test
  public void testAugmentWithNoPrecision_DoubleInput() {
    Double originalValue = new Double(50.03);
    String value = originalValue.toString();
    double lowerBound = 2.1;
    double upperBound = 4.9;

    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setAugmentMask(true);
    configuration.setAugmentLowerBound(lowerBound);
    configuration.setAugmentUpperBound(upperBound);
    configuration.setResultWithPrecision(false);
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      Double maskedDouble = Double.valueOf(maskedValue);
      assertTrue(maskedDouble >= originalValue);
      assertTrue(maskedDouble == (int) maskedDouble.doubleValue());
      assertTrue(maskedDouble <= originalValue + upperBound);
    }
  }

  @Test
  public void testAugmentWithPrecision_IntegerInput_DefaultPrecision() {
    Double originalValue = new Double(50);
    String value = originalValue.toString();
    int lowerBound = 2;
    int upperBound = 4;

    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setAugmentMask(true);
    configuration.setAugmentLowerBound(lowerBound);
    configuration.setAugmentUpperBound(upperBound);
    configuration.setResultWithPrecision(true);
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      assertTrue(maskedValue.contains(".0"));
      Double maskedDouble = Double.valueOf(maskedValue);
      assertTrue(maskedDouble >= originalValue);
      assertTrue(maskedDouble == (int) maskedDouble.doubleValue());
      assertTrue(maskedDouble <= originalValue + upperBound + 1);
    }
  }

  @Test
  public void testAugmentWithPrecision_IntegerInput_PrecisionDigitThree() {
    Double originalValue = new Double(50);
    String value = originalValue.toString();
    double lowerBound = 2;
    double upperBound = 4;

    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setAugmentMask(true);
    configuration.setAugmentLowerBound(lowerBound);
    configuration.setAugmentUpperBound(upperBound);
    configuration.setResultWithPrecision(true);
    configuration.setPrecisionDigits(3);
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      assertTrue(maskedValue.contains(".000"));
      Double maskedDouble = Double.valueOf(maskedValue);
      assertTrue(maskedDouble >= originalValue);
      assertTrue(maskedDouble == (int) maskedDouble.doubleValue());
      assertTrue(maskedDouble <= originalValue + upperBound + 1);
    }
  }

  @Test
  public void testAugmentWithPrecision_DoubleInput_DefaultPrecision() {
    Double originalValue = new Double(50.03);
    String value = originalValue.toString();
    double lowerBound = 2.1;
    double upperBound = 4.9;

    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setAugmentMask(true);
    configuration.setAugmentLowerBound(lowerBound);
    configuration.setAugmentUpperBound(upperBound);
    configuration.setResultWithPrecision(true);
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      Double maskedDouble = Double.valueOf(maskedValue);
      assertTrue(maskedDouble >= originalValue);
      assertTrue(maskedDouble != (int) maskedDouble.doubleValue());
      assertTrue(maskedDouble <= originalValue + upperBound + 1);
    }
  }

  @Test
  public void testAugmentWithPrecision_DoubleInput_PrecisionDigitZero() {
    Double originalValue = new Double(50.03);
    String value = originalValue.toString();
    double lowerBound = 2.1;
    double upperBound = 4.9;

    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setAugmentMask(true);
    configuration.setAugmentLowerBound(lowerBound);
    configuration.setAugmentUpperBound(upperBound);
    configuration.setResultWithPrecision(true);
    configuration.setPrecisionDigits(0);
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      Double maskedDouble = Double.valueOf(maskedValue);
      assertTrue(maskedDouble >= originalValue);
      assertTrue(maskedDouble == (int) maskedDouble.doubleValue());
      assertTrue(maskedDouble <= Math.ceil(originalValue + upperBound + 1));
    }
  }

  @Test
  public void testAugmentWithPrecision_DoubleInput_PrecisionDigitThree() {
    Double originalValue = new Double(50.03);
    String value = originalValue.toString();
    double lowerBound = 2.1;
    double upperBound = 4.9;

    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setAugmentMask(true);
    configuration.setAugmentLowerBound(lowerBound);
    configuration.setAugmentUpperBound(upperBound);
    configuration.setResultWithPrecision(true);
    configuration.setPrecisionDigits(3);
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      Double maskedDouble = Double.valueOf(maskedValue);
      assertTrue(maskedDouble >= originalValue);
      assertTrue(maskedDouble <= Math.ceil(originalValue + upperBound + 1));
    }
  }

  @Test
  public void testAugmentWithNoPrecisionInvalidRange() {
    Double originalValue = new Double(50);
    String value = originalValue.toString();
    int lowerBound = 2;
    int upperBound = 2;

    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setAugmentMask(true);
    configuration.setAugmentLowerBound(lowerBound);
    configuration.setAugmentUpperBound(upperBound);
    configuration.setResultWithPrecision(false);
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      assertNull(maskedValue);
    }
  }

  @Test
  public void testAugmentWithPrecisionInvalidRange() {
    Double originalValue = new Double(50);
    String value = originalValue.toString();
    int lowerBound = 4;
    int upperBound = 2;

    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setAugmentMask(true);
    configuration.setAugmentLowerBound(lowerBound);
    configuration.setAugmentUpperBound(upperBound);
    configuration.setResultWithPrecision(true);
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(value);
      assertNull(maskedValue);
    }
  }

  @Test
  public void testMaskNullNumberVarianceInputReturnNull() throws Exception {
    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    String invalidNumberVariance = null;
    String maskedNumberVariance = maskingProvider.mask(invalidNumberVariance);

    assertEquals(null, maskedNumberVariance);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNumberVarianceInputValidHandlingReturnNull() throws Exception {
    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    String invalidNumberVariance = "Invalid NumberVariance";
    String maskedNumberVariance = maskingProvider.mask(invalidNumberVariance);

    assertEquals(null, maskedNumberVariance);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNumberVarianceInputValidHandlingReturnRandom() throws Exception {
    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    String invalidNumberVariance = "Invalid NumberVariance";
    String maskedNumberVariance = maskingProvider.mask(invalidNumberVariance);

    // return null for this provider
    assertEquals(null, maskedNumberVariance);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNumberVarianceInputValidHandlingReturnDefaultCustomValue()
      throws Exception {
    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    String invalidNumberVariance = "Invalid NumberVariance";
    String maskedNumberVariance = maskingProvider.mask(invalidNumberVariance);

    assertEquals("OTHER", maskedNumberVariance);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNumberVarianceInputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("Test NumberVariance");
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    String invalidNumberVariance = "Invalid NumberVariance";
    String maskedNumberVariance = maskingProvider.mask(invalidNumberVariance);

    assertEquals("Test NumberVariance", maskedNumberVariance);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNumberVarianceInputInvalidHandlingReturnNull() throws Exception {
    NumberVarianceMaskingProviderConfig configuration = new NumberVarianceMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new NumberVarianceMaskingProvider(configuration);

    String invalidNumberVariance = "Invalid NumberVariance";
    String maskedNumberVariance = maskingProvider.mask(invalidNumberVariance);

    assertEquals(null, maskedNumberVariance);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }
}
