/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.Arrays;
import java.util.HashSet;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.ContinentIdentifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.shared.pojo.config.masking.ContinentMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;

public class ContinentMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  @Test
  public void testMaskClosest() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setMaskClosest(true);
    configuration.setMaskClosestK(4);
    MaskingProvider maskingProvider =
        new ContinentMaskingProvider(configuration, tenantId, localizationProperty);

    String originalContinent = "Europe";
    HashSet<String> validNeighbors =
        new HashSet<>(Arrays.asList("Africa", "Asia", "South America", "North America"));

    for (int i = 0; i < 100; i++) {
      String maskedContinent = maskingProvider.mask(originalContinent);
      assertTrue(validNeighbors.contains(maskedContinent));
    }
  }

  @Test
  public void testMask() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setMaskClosest(false);
    ContinentMaskingProvider maskingProvider =
        new ContinentMaskingProvider(configuration, tenantId, localizationProperty);

    boolean changed = false;
    String originalContinent = "Europe";
    for (int i = 0; i < 100; i++) {
      String maskedContinent = maskingProvider.mask(originalContinent);
      assertTrue(maskingProvider.getContinentManager().isValidKey(maskedContinent));
      if (!originalContinent.equalsIgnoreCase(maskedContinent)) {
        changed = true;
      }
    }
    assertTrue(changed);

    // original continent need not be recognized
    originalContinent = "Canada";
    for (int i = 0; i < 100; i++) {
      String maskedContinent = maskingProvider.mask(originalContinent);
      assertNotNull(maskedContinent);
      assertTrue(maskingProvider.getContinentManager().isValidKey(maskedContinent));
    }
  }

  @Test
  public void testMaskNullContinentInputReturnNull() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setMaskClosest(true); // force input recognition
    MaskingProvider maskingProvider =
        new ContinentMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidContinent = null;
    String maskedContinent = maskingProvider.mask(invalidContinent);

    assertEquals(null, maskedContinent);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidContinentInputValidHandlingReturnNull() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setMaskClosest(true); // force input recognition
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    MaskingProvider maskingProvider =
        new ContinentMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidContinent = "Invalid Continent";
    String maskedContinent = maskingProvider.mask(invalidContinent);

    assertEquals(null, maskedContinent);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidContinentInputValidHandlingReturnRandom() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setMaskClosest(true); // force input recognition
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    MaskingProvider maskingProvider =
        new ContinentMaskingProvider(configuration, tenantId, localizationProperty);
    Identifier identifier = new ContinentIdentifier(tenantId, localizationProperty);

    String invalidContinent = "Invalid Continent";
    String maskedContinent = maskingProvider.mask(invalidContinent);

    assertNotNull(maskedContinent);
    assertTrue(identifier.isOfThisType(maskedContinent));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidContinentInputValidHandlingReturnDefaultCustomValue()
      throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setMaskClosest(true); // force input recognition
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    MaskingProvider maskingProvider =
        new ContinentMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidContinent = "Invalid Continent";
    String maskedContinent = maskingProvider.mask(invalidContinent);

    assertEquals("OTHER", maskedContinent);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidContinentInputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setMaskClosest(true); // force input recognition
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    configuration.setUnexpectedInputReturnMessage("Test Continent");
    MaskingProvider maskingProvider =
        new ContinentMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidContinent = "Invalid Continent";
    String maskedContinent = maskingProvider.mask(invalidContinent);

    assertEquals("Test Continent", maskedContinent);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidContinentInputInvalidHandlingReturnNull() throws Exception {
    ContinentMaskingProviderConfig configuration = new ContinentMaskingProviderConfig();
    configuration.setMaskClosest(true); // force input recognition
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.ERROR_EXIT);
    MaskingProvider maskingProvider =
        new ContinentMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidContinent = "Invalid Continent";
    try {
      maskingProvider.mask(invalidContinent);
      fail("expected exception");
    } catch (PrivacyProviderInvalidInputException e) {
      assertTrue(e.getMessage().contains(invalidContinent));
    }
  }

  @Test
  @Ignore
  public void testPerformance() {
    ContinentMaskingProviderConfig defaultConfiguration = new ContinentMaskingProviderConfig();
    ContinentMaskingProviderConfig closestConfiguration = new ContinentMaskingProviderConfig();
    closestConfiguration.setMaskClosest(true);

    ContinentMaskingProviderConfig[] configurations =
        new ContinentMaskingProviderConfig[] {defaultConfiguration // ,
        // closestConfiguration
        };

    String originalContinent = "Europe";

    for (ContinentMaskingProviderConfig maskingConfiguration : configurations) {
      ContinentMaskingProvider maskingProvider =
          new ContinentMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

      int N = 1000000;

      long startMillis = System.currentTimeMillis();

      for (int i = 0; i < N; i++) {
        maskingProvider.mask(originalContinent);
      }

      long diff = System.currentTimeMillis() - startMillis;
      System.out.println(String.format(" %d operations took %d milliseconds (%f per op)", N, diff,
          (double) diff / N));
      // Assert test always should finish in less than 10 seconds
      assertTrue(diff < 10000);
    }
  }

}
