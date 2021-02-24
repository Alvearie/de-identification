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

import com.ibm.whc.deid.providers.identifiers.ICDv10Identifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.shared.pojo.config.masking.ICDv10MaskingProviderConfig;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class ICDv10MaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {
  /*
   * Tests for both randomize chapter and category options and their boolean values (true and
   * false). That is, tests converting the ICDv10 code or name to category or chapter. It also tests
   * for format, case , and invalid value.
   */

	private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

  @Test
  public void testMaskConvertToCategory() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();
    MaskingProvider maskingProvider = new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);

    // default configuration is to convert to category
    String originalICD = "A01.0";
    String maskedICD = maskingProvider.mask(originalICD);
    assertFalse(originalICD.equals(maskedICD));
    assertTrue(maskedICD.equals("A00-A09"));

    originalICD = "Typhoid Fever";
    maskedICD = maskingProvider.mask(originalICD);
    assertFalse(originalICD.equals(maskedICD));
    assertTrue(maskedICD.equals("Intestinal infectious diseases"));
  }

  @Ignore
  @Test
  public void testPerformance() {
    ICDv10MaskingProviderConfig defaultMaskingConfiguration = new ICDv10MaskingProviderConfig();
    ICDv10MaskingProviderConfig chapterConfiguration = new ICDv10MaskingProviderConfig();

    defaultMaskingConfiguration.setRandomizeCategory(false);

    chapterConfiguration.setRandomizeCategory(false);
    chapterConfiguration.setRandomizeChapter(true);

    ICDv10MaskingProviderConfig[] configurations =
        new ICDv10MaskingProviderConfig[] {defaultMaskingConfiguration, chapterConfiguration};

    for (ICDv10MaskingProviderConfig configuration : configurations) {

      MaskingProvider maskingProvider = new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);

      int N = 1000000;
      String[] originalICDs = {"A01.0", "Typhoid Fever"};

      for (String originalICD : originalICDs) {

        long startMillis = System.currentTimeMillis();

        for (int i = 0; i < N; i++) {
          maskingProvider.mask(originalICD);
        }

        long diff = System.currentTimeMillis() - startMillis;
        System.out.println(String.format(" %s: %d operations took %d milliseconds (%f per op)",
            originalICD, N, diff, (double) diff / N));
        // Assert test always should finish in less than 10 seconds
        assertTrue(diff < 10000);
      }
    }
  }

  @Test
  public void testMaskConvertToChapter() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();

    configuration.setRandomizeCategory(false);
    configuration.setRandomizeChapter(true);
    MaskingProvider maskingProvider = new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);

    // default configuration is to convert to category
    String originalICD = "A01.0";
    String maskedICD = maskingProvider.mask(originalICD);
    assertFalse(originalICD.equals(maskedICD));
    assertTrue(maskedICD.equals("A00-B99"));

    // test that format is preserved
    originalICD = "Typhoid Fever";
    maskedICD = maskingProvider.mask(originalICD);
    assertFalse(originalICD.equals(maskedICD));
    assertTrue(maskedICD.equals("Certain infectious and parasitic diseases"));

    // check that case is ignored
    originalICD = "Typhoid Fever".toLowerCase();
    maskedICD = maskingProvider.mask(originalICD);
    assertFalse(originalICD.equals(maskedICD));
    assertTrue(maskedICD.equals("Certain infectious and parasitic diseases"));
  }

  @Test
  public void testMaskConvertToRandom() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();

    configuration.setRandomizeCategory(false);
    configuration.setRandomizeChapter(false);

    MaskingProvider maskingProvider = new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);

    // Test convert to chapter
    String originalICD = "002.0";
    String maskedICD = maskingProvider.mask(originalICD);
    assertEquals(null, maskedICD);

    // test that format is preserved
    originalICD = "Typhoid Fever";
    maskedICD = maskingProvider.mask(originalICD);
    assertFalse(maskedICD.isEmpty());

    // check that case is ignored
    originalICD = "Typhoid Fever".toLowerCase();
    maskedICD = maskingProvider.mask(originalICD);
    assertFalse(maskedICD.isEmpty());
  }

  @Test
  public void testMaskNullICDv10InputReturnNull() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();

    MaskingProvider maskingProvider = new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);

    String invalidICDv10 = null;
    String maskedICDv10 = maskingProvider.mask(invalidICDv10);

    assertEquals(null, maskedICDv10);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidICDv10InputValidHandlingReturnNull() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider = new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);

    String invalidICDv10 = "Invalid ICDv10";
    String maskedICDv10 = maskingProvider.mask(invalidICDv10);

    assertEquals(null, maskedICDv10);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidICDv10InputValidHandlingReturnRandom() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider = new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);
		Identifier identifier = new ICDv10Identifier(tenantId, localizationProperty);

    String invalidICDv10 = "Invalid ICDv10";
    String maskedICDv10 = maskingProvider.mask(invalidICDv10);

    assertFalse(maskedICDv10.equals(invalidICDv10));
    assertTrue(identifier.isOfThisType(maskedICDv10));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidICDv10InputValidHandlingReturnDefaultCustomValue() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);

    String invalidICDv10 = "Invalid ICDv10";
    String maskedICDv10 = maskingProvider.mask(invalidICDv10);

    assertEquals("OTHER", maskedICDv10);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidICDv10InputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("Test ICDv10");
    MaskingProvider maskingProvider = new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);

    String invalidICDv10 = "Invalid ICDv10";
    String maskedICDv10 = maskingProvider.mask(invalidICDv10);

    assertEquals("Test ICDv10", maskedICDv10);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidICDv10InputInvalidHandlingReturnNull() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);

    String invalidICDv10 = "Invalid ICDv10";
    String maskedICDv10 = maskingProvider.mask(invalidICDv10);

    assertEquals(null, maskedICDv10);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

}
