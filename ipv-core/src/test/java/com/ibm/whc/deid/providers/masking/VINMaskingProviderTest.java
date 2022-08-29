/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.VINIdentifier;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
import com.ibm.whc.deid.shared.pojo.config.masking.VINMaskingProviderConfig;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.VINManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class VINMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {
  private final VINIdentifier vinIdentifier = new VINIdentifier(tenantId, localizationProperty);


  /*
   * Tests for both VIN options and their boolean values (true and false). Also tests for an invalid
   * value.
   */

  @Test
  public void testDefaultMask() {
    VINMaskingProviderConfig configuration = new VINMaskingProviderConfig();
    MaskingProvider vinMaskingProvider =
        new VINMaskingProvider(configuration, tenantId, localizationProperty);

    String vin = "1B312345678901234";
    assertTrue(vinIdentifier.isOfThisType(vin));
    String maskedResult = vinMaskingProvider.mask(vin);
    // default is to preserve WMI and VDS
    assertTrue(vinIdentifier.isOfThisType(maskedResult));
    assertTrue(maskedResult.startsWith("1B3"));
    assertFalse(maskedResult.equals(vin));
  }

  @Test
  public void testWMIFlagOff() {
    VINMaskingProviderConfig configuration = new VINMaskingProviderConfig();
    configuration.setWmiPreserve(false);
    configuration.setVdsPreserve(true);
    MaskingProvider vinMaskingProvider =
        new VINMaskingProvider(configuration, tenantId, localizationProperty);
    VINManager vinManager = (VINManager) ManagerFactory.getInstance().getManager(null,
        Resource.WORLD_MANUFACTURERS_IDENTIFIER, null,
        LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);

    String vin = "1B312345678901234";
    String maskedResult = vinMaskingProvider.mask(vin);

    assertTrue(vinIdentifier.isOfThisType(maskedResult));
    assertFalse(maskedResult.equals(vin));

    String randomWMI = maskedResult.substring(0, 3);
    assertFalse(randomWMI.equals("1B3"));
    assertTrue(vinManager.isValidWMI(randomWMI));
  }

  @Test
  public void testVDSFlagOff() {
    VINMaskingProviderConfig configuration = new VINMaskingProviderConfig();
    configuration.setWmiPreserve(true);
    configuration.setVdsPreserve(false);
    MaskingProvider vinMaskingProvider =
        new VINMaskingProvider(configuration, tenantId, localizationProperty);

    String vin = "1B312345678901234";
    String maskedResult = vinMaskingProvider.mask(vin);

    assertTrue(vinIdentifier.isOfThisType(maskedResult));
    assertFalse(maskedResult.equals(vin));

    String randomVDS = maskedResult.substring(3, 9);
    String originalVDS = vin.substring(3, 9);
    assertFalse(originalVDS.equals(randomVDS));
  }

  @Test
  @Ignore
  public void testPerformance() {
    int N = 1000000;
    VINMaskingProviderConfig defaultConfiguration = new VINMaskingProviderConfig();
    VINMaskingProviderConfig nopreserveConfiguration = new VINMaskingProviderConfig();
    nopreserveConfiguration.setWmiPreserve(false);
    nopreserveConfiguration.setVdsPreserve(false);

    VINMaskingProviderConfig[] configurations =
        new VINMaskingProviderConfig[] {defaultConfiguration, nopreserveConfiguration};

    String[] originalValues = new String[] {"1B312345678901234"};

    for (VINMaskingProviderConfig maskingConfiguration : configurations) {
      VINMaskingProvider maskingProvider =
          new VINMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

      for (String originalValue : originalValues) {
        long startMillis = System.currentTimeMillis();

        for (int i = 0; i < N; i++) {
          maskingProvider.mask(originalValue);
        }

        long diff = System.currentTimeMillis() - startMillis;
        System.out.println(String.format(" %s: %d operations took %d milliseconds (%f per op)",
            originalValue, N, diff, (double) diff / N));
        // Assert test always should finish in less than 10 seconds
        assertTrue(diff < 10000);
      }
    }
  }

  @Test
  public void testMaskNullVINInputReturnNull() throws Exception {
    VINMaskingProviderConfig configuration = new VINMaskingProviderConfig();
    MaskingProvider maskingProvider =
        new VINMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidVIN = null;
    String maskedVIN = maskingProvider.mask(invalidVIN);

    assertEquals(null, maskedVIN);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidVINInputValidHandlingReturnNull() throws Exception {
    VINMaskingProviderConfig configuration = new VINMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    MaskingProvider maskingProvider =
        new VINMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidVIN = "Invalid VIN";
    String maskedVIN = maskingProvider.mask(invalidVIN);

    assertEquals(null, maskedVIN);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidVINInputValidHandlingReturnRandom() throws Exception {
    VINMaskingProviderConfig configuration = new VINMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    MaskingProvider maskingProvider =
        new VINMaskingProvider(configuration, tenantId, localizationProperty);
    Identifier identifier = new VINIdentifier(tenantId, localizationProperty);

    String invalidVIN = "Invalid VIN";
    String maskedVIN = maskingProvider.mask(invalidVIN);

    assertFalse(maskedVIN.equals(invalidVIN));
    assertTrue(identifier.isOfThisType(maskedVIN));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidVINInputValidHandlingReturnDefaultCustomValue() throws Exception {
    VINMaskingProviderConfig configuration = new VINMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    MaskingProvider maskingProvider =
        new VINMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidVIN = "Invalid VIN";
    String maskedVIN = maskingProvider.mask(invalidVIN);

    assertEquals("OTHER", maskedVIN);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidVINInputValidHandlingReturnNonDefaultCustomValue() throws Exception {
    VINMaskingProviderConfig configuration = new VINMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    configuration.setUnexpectedInputReturnMessage("Test VIN");
    MaskingProvider maskingProvider =
        new VINMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidVIN = "Invalid VIN";
    String maskedVIN = maskingProvider.mask(invalidVIN);

    assertEquals("Test VIN", maskedVIN);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

}
