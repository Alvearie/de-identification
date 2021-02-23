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
import com.ibm.whc.deid.providers.identifiers.RaceEthnicityIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.RaceEthnicityMaskingProviderConfig;

public class RaceEthnicityMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {
  /*
   * Race masking provider has no options. It tests for random masking of race value. It also tests
   * for localization masking of race value.
   */

  @Test
  public void testMask() throws Exception {
    RaceEthnicityMaskingProviderConfig configuration = new RaceEthnicityMaskingProviderConfig();
    MaskingProvider maskingProvider = new RaceEthnicityMaskingProvider(configuration, tenantId);
    RaceEthnicityIdentifier identifier = new RaceEthnicityIdentifier();

    String originalRace = "white";

    int randomizationOK = 0;

    for (int i = 0; i < 10; i++) {
      String maskedRace = maskingProvider.mask(originalRace);
      assertTrue(identifier.isOfThisType(maskedRace));
      if (!maskedRace.equals(originalRace)) {
        randomizationOK++;
      }
    }
    assertTrue(randomizationOK > 0);
  }


  @Test
  public void testMaskNullRaceEthnicityInputReturnNull() throws Exception {
    RaceEthnicityMaskingProviderConfig configuration = new RaceEthnicityMaskingProviderConfig();
    MaskingProvider maskingProvider = new RaceEthnicityMaskingProvider(configuration, tenantId);

    String invalidRaceEthnicity = null;
    String maskedRaceEthnicity = maskingProvider.mask(invalidRaceEthnicity);

    assertEquals(null, maskedRaceEthnicity);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidRaceEthnicityInputValidHandlingReturnNull() throws Exception {
    RaceEthnicityMaskingProviderConfig configuration = new RaceEthnicityMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider = new RaceEthnicityMaskingProvider(configuration, tenantId);

    String invalidRaceEthnicity = "Invalid RaceEthnicity";
    String maskedRaceEthnicity = maskingProvider.mask(invalidRaceEthnicity);

    assertEquals(null, maskedRaceEthnicity);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidRaceEthnicityInputValidHandlingReturnRandom() throws Exception {
    RaceEthnicityMaskingProviderConfig configuration = new RaceEthnicityMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider = new RaceEthnicityMaskingProvider(configuration, tenantId);
    Identifier identifier = new RaceEthnicityIdentifier();

    String invalidRaceEthnicity = "Invalid RaceEthnicity";
    String maskedRaceEthnicity = maskingProvider.mask(invalidRaceEthnicity);

    assertFalse(maskedRaceEthnicity.equals(invalidRaceEthnicity));
    assertTrue(identifier.isOfThisType(maskedRaceEthnicity));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidRaceEthnicityInputValidHandlingReturnDefaultCustomValue()
      throws Exception {
    RaceEthnicityMaskingProviderConfig configuration = new RaceEthnicityMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new RaceEthnicityMaskingProvider(configuration, tenantId);

    String invalidRaceEthnicity = "Invalid RaceEthnicity";
    String maskedRaceEthnicity = maskingProvider.mask(invalidRaceEthnicity);

    assertEquals("OTHER", maskedRaceEthnicity);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidRaceEthnicityInputValidHandlingReturnNonDefaultCustomValue() {
    RaceEthnicityMaskingProviderConfig configuration = new RaceEthnicityMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("Test RaceEthnicity");
    MaskingProvider maskingProvider = new RaceEthnicityMaskingProvider(configuration, tenantId);

    String invalidRaceEthnicity = "Invalid RaceEthnicity";
    String maskedRaceEthnicity = maskingProvider.mask(invalidRaceEthnicity);

    assertEquals("Test RaceEthnicity", maskedRaceEthnicity);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidRaceEthnicityInputInvalidHandlingReturnNull() throws Exception {
    RaceEthnicityMaskingProviderConfig configuration = new RaceEthnicityMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new RaceEthnicityMaskingProvider(configuration, tenantId);

    String invalidRaceEthnicity = "Invalid RaceEthnicity";
    String maskedRaceEthnicity = maskingProvider.mask(invalidRaceEthnicity);

    assertEquals(null, maskedRaceEthnicity);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  @Ignore
  public void testPerformance() {
    int N = 1000000;
    RaceEthnicityMaskingProviderConfig defaultConfiguration =
        new RaceEthnicityMaskingProviderConfig();

    RaceEthnicityMaskingProviderConfig[] configurations =
        new RaceEthnicityMaskingProviderConfig[] {defaultConfiguration};

    String[] originalValues = new String[] {"white"};

    for (RaceEthnicityMaskingProviderConfig maskingConfiguration : configurations) {
      RaceEthnicityMaskingProvider maskingProvider =
          new RaceEthnicityMaskingProvider(maskingConfiguration, tenantId);

      for (String originalValue : originalValues) {
        long startMillis = System.currentTimeMillis();

        for (int i = 0; i < N; i++) {
          maskingProvider.mask(originalValue);
        }

        long diff = System.currentTimeMillis() - startMillis;
        System.out.println(String.format("%s: %d operations took %d milliseconds (%f per op)",
            originalValue, N, diff, (double) diff / N));
        // Assert test always should finish in less than 10 seconds
        assertTrue(diff < 10000);
      }
    }
  }

}
