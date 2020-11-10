/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.ATCMaskingProviderConfig;

public class ATCMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {
  // @formatter:off
  /*
   * Tests for mask levelsToKeep option for all 5 level values (1 through 5).
   * It also tests for an invalid value.
   */

  @Test
  public void testMask() {
    ATCMaskingProviderConfig configuration = new ATCMaskingProviderConfig();
    configuration.setMaskLevelsToKeep(1);
    MaskingProvider maskingProvider = new ATCMaskingProvider(configuration, tenantId);

    String atc = "A04AA02";
    String maskedValue = maskingProvider.mask(atc);
    assertTrue(maskedValue.equals("A"));

    configuration.setMaskLevelsToKeep(2);
    maskingProvider = new ATCMaskingProvider(configuration, tenantId);
    maskedValue = maskingProvider.mask(atc);
    assertTrue(maskedValue.equals("A04"));

    configuration.setMaskLevelsToKeep(3);
    maskingProvider = new ATCMaskingProvider(configuration, tenantId);
    maskedValue = maskingProvider.mask(atc);
    assertTrue(maskedValue.equals("A04A"));

    configuration.setMaskLevelsToKeep(4);
    maskingProvider = new ATCMaskingProvider(configuration, tenantId);
    maskedValue = maskingProvider.mask(atc);
    assertTrue(maskedValue.equals("A04AA"));

    configuration.setMaskLevelsToKeep(5);
    maskingProvider = new ATCMaskingProvider(configuration, tenantId);
    maskedValue = maskingProvider.mask(atc);
    assertEquals(null, maskedValue);
  }

  @Test
  public void testMaskNullValueReturnNull() {
    ATCMaskingProviderConfig configuration = new ATCMaskingProviderConfig();
    MaskingProvider maskingProvider = new ATCMaskingProvider(configuration, tenantId);

    String atc = null;
    String maskedValue = maskingProvider.mask(atc);
    assertEquals(null, maskedValue);
    assertThat(outContent.toString(), containsString("WARN - WPH1011W - "));
    assertThat(
        outContent.toString(),
        containsString("The value \"identifier\" for \"ATC masking\" is NULL"));
  }

  @Test
  public void testMaskInvalidValueValidHandlingReturnNull() {
    ATCMaskingProviderConfig configuration = new ATCMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider = new ATCMaskingProvider(configuration, tenantId);

    String atc = "foobar";
    String maskedValue = maskingProvider.mask(atc);
    assertEquals(null, maskedValue);
    assertThat(outContent.toString(), containsString("WARN - WPH1011W - "));
    assertThat(
        outContent.toString(),
        containsString("The value \"identifier\" for \"ATC masking\" is NULL"));
  }

  @Test
  public void testMaskInvalidValueValidHandlingReturnRandom() {
    ATCMaskingProviderConfig configuration = new ATCMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider = new ATCMaskingProvider(configuration, tenantId);

    String atc = "foobar";
    String maskedValue = maskingProvider.mask(atc);
    assertEquals(null, maskedValue);
    assertThat(outContent.toString(), containsString("WARN - WPH1011W - "));
    assertThat(
        outContent.toString(),
        containsString("The value \"identifier\" for \"ATC masking\" is NULL"));
  }

  @Test
  public void testMaskInvalidValueValidHandlingReturnDefaultCustomValue() {
    ATCMaskingProviderConfig configuration = new ATCMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new ATCMaskingProvider(configuration, tenantId);

    String atc = "foobar";
    String maskedValue = maskingProvider.mask(atc);
    assertEquals("OTHER", maskedValue);
    assertThat(outContent.toString(), containsString("WARN - WPH1011W - "));
    assertThat(
        outContent.toString(),
        containsString("The value \"identifier\" for \"ATC masking\" is NULL"));
  }

  @Test
  public void testMaskInvalidValueValidHandlingReturnNonDefaultCustomValue() {
    ATCMaskingProviderConfig configuration = new ATCMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("Test ATC");
    MaskingProvider maskingProvider = new ATCMaskingProvider(configuration, tenantId);

    String atc = "foobar";
    String maskedValue = maskingProvider.mask(atc);
    assertEquals("Test ATC", maskedValue);
    assertThat(outContent.toString(), containsString("WARN - WPH1011W - "));
    assertThat(
        outContent.toString(),
        containsString("The value \"identifier\" for \"ATC masking\" is NULL"));
  }

  @Test
  public void testMaskInvalidValueInvalidHandlingReturnNull() {
    ATCMaskingProviderConfig configuration = new ATCMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new ATCMaskingProvider(configuration, tenantId);

    String atc = "foobar";
    String maskedValue = maskingProvider.mask(atc);
    assertEquals(null, maskedValue);
    assertThat(outContent.toString(), containsString("WARN - WPH1011W - "));
    assertThat(
        outContent.toString(),
        containsString("The value \"identifier\" for \"ATC masking\" is NULL"));
  }

}
