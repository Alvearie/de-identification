/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.Arrays;
import java.util.HashSet;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.CityIdentifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.shared.pojo.config.masking.CityMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.util.CityManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class CityMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  protected static BasicMaskingProviderFactory maskingProviderFactory;

  @BeforeClass
  public static void setup() {
    maskingProviderFactory = new BasicMaskingProviderFactory();
  }

  /*
   * Tests all three of the CityMaskingProvider options (maskClosest, maskClosestK, and
   * maskPseudorandom).
   */

  @Test
  public void testMask() {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    CityMaskingProvider maskingProvider =
        (CityMaskingProvider) maskingProviderFactory.getProviderFromType(MaskingProviderType.CITY,
            null, maskingConfiguration, tenantId, localizationProperty);

    // different values
    String originalCity = "Dublin";
    boolean changed = false;
    for (int i = 0; i < 20; i++) {
      String maskedCity = maskingProvider.mask(originalCity);
      if (!maskedCity.equalsIgnoreCase(originalCity)) {
        changed = true;
      }
      assertTrue(maskingProvider.getCityManager().isValidKey(maskedCity));
    }
    assertTrue(changed);
    CityManager cityManager = maskingProvider.getCityManager();

    // original city need not be recognized
    originalCity = "XXX$$$";
    for (int i = 0; i < 20; i++) {
      String maskedCity = maskingProvider.mask(originalCity);
      assertNotNull(maskedCity);
      assertTrue(maskingProvider.getCityManager().isValidKey(maskedCity));
    }
    assertTrue(changed);
    assertSame(cityManager, maskingProvider.getCityManager());
  }

  @Test
  public void testPseudorandom() throws Exception {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    maskingConfiguration.setMaskPseudorandom(true);

    CityMaskingProvider maskingProvider =
        (CityMaskingProvider) maskingProviderFactory.getProviderFromType(MaskingProviderType.CITY,
            null, maskingConfiguration, tenantId, localizationProperty);

    String originalCity = "Dublin";
    String maskedCity = maskingProvider.mask(originalCity);
    String firstMask = maskedCity;
    for (int i = 0; i < 10; i++) {
      maskedCity = maskingProvider.mask(originalCity);
      assertEquals(firstMask, maskedCity);
    }

    // original city need not be recognized
    originalCity = "XXX$$$";
    maskedCity = maskingProvider.mask(originalCity);
    firstMask = maskedCity;
    for (int i = 0; i < 10; i++) {
      maskedCity = maskingProvider.mask(originalCity);
      assertEquals(firstMask, maskedCity);
    }
  }


  @Test
  public void testMaskNullCityInputReturnNull() throws Exception {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    CityMaskingProvider maskingProvider =
        (CityMaskingProvider) maskingProviderFactory.getProviderFromType(MaskingProviderType.CITY,
            null, maskingConfiguration, tenantId, localizationProperty);

    String invalidCity = null;
    String maskedCity = maskingProvider.mask(invalidCity);

    assertEquals(null, maskedCity);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCityInputValidHandlingReturnNull() throws Exception {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    maskingConfiguration.setMaskClosest(true); // force input recognition
    CityMaskingProvider maskingProvider =
        (CityMaskingProvider) maskingProviderFactory.getProviderFromType(MaskingProviderType.CITY,
            null, maskingConfiguration, tenantId, localizationProperty);
    String invalidCity = "Invalid City";
    String maskedCity = maskingProvider.mask(invalidCity);

    assertEquals(null, maskedCity);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCityInputValidHandlingReturnRandom() throws Exception {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    maskingConfiguration.setMaskClosest(true); // force input recognition
    maskingConfiguration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    CityMaskingProvider maskingProvider =
        (CityMaskingProvider) maskingProviderFactory.getProviderFromType(MaskingProviderType.CITY,
            null, maskingConfiguration, tenantId,
            LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);
    Identifier identifier = new CityIdentifier(tenantId, localizationProperty);

    String invalidCity = "Invalid City";
    String maskedCity = maskingProvider.mask(invalidCity);

    assertFalse(maskedCity.equals(invalidCity));
    assertTrue(identifier.isOfThisType(maskedCity));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCityInputValidHandlingReturnDefaultCustomValue() throws Exception {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    maskingConfiguration.setMaskClosest(true); // force input recognition
    maskingConfiguration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    CityMaskingProvider maskingProvider =
        (CityMaskingProvider) maskingProviderFactory.getProviderFromType(MaskingProviderType.CITY,
            null, maskingConfiguration, tenantId, localizationProperty);
    String invalidCity = "Invalid City";
    String maskedCity = maskingProvider.mask(invalidCity);

    assertEquals("OTHER", maskedCity);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCityInputValidHandlingReturnNonDefaultCustomValue() throws Exception {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    maskingConfiguration.setMaskClosest(true); // force input recognition
    maskingConfiguration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    maskingConfiguration.setUnexpectedInputReturnMessage("Test City");
    CityMaskingProvider maskingProvider =
        (CityMaskingProvider) maskingProviderFactory.getProviderFromType(MaskingProviderType.CITY,
            null, maskingConfiguration, tenantId, localizationProperty);

    String invalidCity = "Invalid City";
    String maskedCity = maskingProvider.mask(invalidCity);

    assertEquals("Test City", maskedCity);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidCityInputInvalidHandlingReturnErrorExit() throws Exception {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    maskingConfiguration.setMaskClosest(true); // force input recognition
    maskingConfiguration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.ERROR_EXIT);
    CityMaskingProvider maskingProvider =
        (CityMaskingProvider) maskingProviderFactory.getProviderFromType(MaskingProviderType.CITY,
            null, maskingConfiguration, tenantId, localizationProperty);
    String invalidCity = "Invalid City";
    try {
      maskingProvider.mask(invalidCity);
      fail("expected exception");
    } catch (PrivacyProviderInvalidInputException e) {
      assertTrue(e.getMessage().contains(invalidCity));
    }
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  @Ignore
  public void testPerformance() {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    CityMaskingProviderConfig closestConfiguration = new CityMaskingProviderConfig();
    closestConfiguration.setMaskClosest(true);

    CityMaskingProviderConfig[] configurations = {maskingConfiguration, closestConfiguration};

    String originalCity = "Dublin";

    for (CityMaskingProviderConfig config : configurations) {
      CityMaskingProvider maskingProvider =
          new CityMaskingProvider(config, tenantId, localizationProperty);

      int N = 1000000;

      long startMillis = System.currentTimeMillis();

      for (int i = 0; i < N; i++) {
        maskingProvider.mask(originalCity);
      }

      long diff = System.currentTimeMillis() - startMillis;
      System.out.println(String.format(" %d operations took %d milliseconds (%f per op)", N, diff,
          (double) diff / N));
      // Assert test always should finish in less than 10 seconds
      assertTrue(diff < 10000);
    }
  }

  @Test
  public void testMaskClosest() {
    CityMaskingProviderConfig maskingConfiguration = new CityMaskingProviderConfig();
    maskingConfiguration.setMaskClosest(true);
    maskingConfiguration.setMaskClosestK(4);
    maskingConfiguration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);

    CityMaskingProvider maskingProvider =
        (CityMaskingProvider) maskingProviderFactory.getProviderFromType(MaskingProviderType.CITY,
            null, maskingConfiguration, tenantId, TEST_LOCALIZATION_PROPERTIES);

    String originalCity = "rochester";
    HashSet<String> neighbors =
        new HashSet<>(Arrays.asList("Minneapolis", "Byron", "Kasson", "Mantorville"));

    HashSet<String> selected = new HashSet<>();
    for (int i = 0; i < 100; i++) {
      String maskedCity = maskingProvider.mask(originalCity);
      assertNotNull(maskedCity);
      assertTrue(maskedCity, neighbors.contains(maskedCity));
      selected.add(maskedCity);
    }
    // System.out.println(selected);

    // not recognized - apply unexpected value handling
    originalCity = "DublinXXX";
    String value = maskingProvider.mask(originalCity);
    assertEquals("OTHER", value);
    assertFalse(maskingProvider.getCityManager().isValidKey(originalCity));
    
    // no location available - apply unexpected value handling
    originalCity = "Pittsburg";
    value = maskingProvider.mask(originalCity);
    assertEquals("OTHER", value);
    assertTrue(maskingProvider.getCityManager().isValidKey(originalCity));
  }
}
