/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.ICDv9Identifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.shared.pojo.config.masking.ICDv9MaskingProviderConfig;

public class ICDv9MaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  /*
   * Tests for both randomize chapter and category options and their boolean values (true and
   * false). That is, tests converting the ICDv9 code or name to category or chapter. It also tests
   * for format, case , and invalid value.
   */

  @Test
  public void testMaskConvertToCategory() throws Exception {
    ICDv9MaskingProviderConfig configuration = new ICDv9MaskingProviderConfig();
    MaskingProvider maskingProvider =
        new ICDv9MaskingProvider(configuration, tenantId, localizationProperty);
    // default configuration is to convert to category
    String originalICD = "002.0";
    String maskedICD = maskingProvider.mask(originalICD);
    assertFalse(originalICD.equals(maskedICD));
    assertTrue(maskedICD.equals("002"));

    originalICD = "Typhoid Fever";
    maskedICD = maskingProvider.mask(originalICD);
    assertFalse(originalICD.equals(maskedICD));
    assertTrue(maskedICD.equals("Typhoid and paratyphoid fevers"));
  }

  @Test
  @Ignore
  public void testPerformance() {
    ICDv9MaskingProviderConfig defaultMaskingConfiguration = new ICDv9MaskingProviderConfig();
    ICDv9MaskingProviderConfig chapterConfiguration = new ICDv9MaskingProviderConfig();

    defaultMaskingConfiguration.setRandomizeCategory(false);

    chapterConfiguration.setRandomizeCategory(false);
    chapterConfiguration.setRandomizeChapter(true);

    ICDv9MaskingProviderConfig[] configurations =
        new ICDv9MaskingProviderConfig[] {defaultMaskingConfiguration, chapterConfiguration};

    for (ICDv9MaskingProviderConfig configuration : configurations) {

      ICDv9MaskingProvider maskingProvider =
          new ICDv9MaskingProvider(configuration, tenantId, localizationProperty);

      int N = 1000000;
      String[] originalICDs = {"002.0", "Typhoid Fever"};

      for (String originalICD : originalICDs) {

        long startMillis = System.currentTimeMillis();

        for (int i = 0; i < N; i++) {
          maskingProvider.mask(originalICD);
        }

        long diff = System.currentTimeMillis() - startMillis;
        System.out.println(String.format("%s: %d operations took %d milliseconds (%f per op)",
            originalICD, N, diff, (double) diff / N));
        // Assert test always should finish in less than 10 seconds
        assertTrue(diff < 10000);
      }
    }
  }

  @Test
  public void testMaskConvertToChapter() {
    ICDv9MaskingProviderConfig configuration = new ICDv9MaskingProviderConfig();

    configuration.setRandomizeCategory(false);
    configuration.setRandomizeChapter(true);

    MaskingProvider maskingProvider =
        new ICDv9MaskingProvider(configuration, tenantId, localizationProperty);

    // Test convert to chapter
    String originalICD = "002.0";
    String maskedICD = maskingProvider.mask(originalICD);
    assertFalse(originalICD.equals(maskedICD));
    assertTrue(maskedICD.equals("001-139"));

    // test that format is preserved
    originalICD = "Typhoid Fever";
    maskedICD = maskingProvider.mask(originalICD);
    assertFalse(originalICD.equals(maskedICD));
    assertTrue(maskedICD.equals("infectious and parasitic diseases"));

    // check that case is ignored
    originalICD = "Typhoid Fever".toLowerCase();
    maskedICD = maskingProvider.mask(originalICD);
    assertFalse(originalICD.equals(maskedICD));
    assertTrue(maskedICD.equals("infectious and parasitic diseases"));
  }

  @Test
  public void testMaskConvertToRandom() throws Exception {
    ICDv9MaskingProviderConfig configuration = new ICDv9MaskingProviderConfig();

    configuration.setRandomizeCategory(false);
    configuration.setRandomizeChapter(false);

    MaskingProvider maskingProvider =
        new ICDv9MaskingProvider(configuration, tenantId, localizationProperty);

    // Test convert to chapter
    String originalICD = "002.0";
    String maskedICD = maskingProvider.mask(originalICD);
    assertFalse(maskedICD.isEmpty());

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
  public void testMaskNullICDv9InputReturnNull() throws Exception {
    ICDv9MaskingProviderConfig configuration = new ICDv9MaskingProviderConfig();

    MaskingProvider maskingProvider =
        new ICDv9MaskingProvider(configuration, tenantId, localizationProperty);

    String invalidICDv9 = null;
    String maskedICDv9 = maskingProvider.mask(invalidICDv9);

    assertEquals(null, maskedICDv9);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidICDv9InputValidHandlingReturnNull() throws Exception {
    ICDv9MaskingProviderConfig configuration = new ICDv9MaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider =
        new ICDv9MaskingProvider(configuration, tenantId, localizationProperty);

    String invalidICDv9 = "Invalid ICDv9";
    String maskedICDv9 = maskingProvider.mask(invalidICDv9);

    assertEquals(null, maskedICDv9);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidICDv9InputValidHandlingReturnRandom() throws Exception {
    ICDv9MaskingProviderConfig configuration = new ICDv9MaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider =
        new ICDv9MaskingProvider(configuration, tenantId, localizationProperty);
    Identifier identifier = new ICDv9Identifier(tenantId, localizationProperty);

    String invalidICDv9 = "Invalid ICDv9";
    String maskedICDv9 = maskingProvider.mask(invalidICDv9);

    assertFalse(maskedICDv9.equals(invalidICDv9));
    assertTrue(identifier.isOfThisType(maskedICDv9));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidICDv9InputValidHandlingReturnDefaultCustomValue() throws Exception {
    ICDv9MaskingProviderConfig configuration = new ICDv9MaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider =
        new ICDv9MaskingProvider(configuration, tenantId, localizationProperty);

    String invalidICDv9 = "Invalid ICDv9";
    String maskedICDv9 = maskingProvider.mask(invalidICDv9);

    assertEquals("OTHER", maskedICDv9);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidICDv9InputValidHandlingReturnNonDefaultCustomValue() {
    ICDv9MaskingProviderConfig configuration = new ICDv9MaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("Test ICDv9");
    MaskingProvider maskingProvider =
        new ICDv9MaskingProvider(configuration, tenantId, localizationProperty);

    String invalidICDv9 = "Invalid ICDv9";
    String maskedICDv9 = maskingProvider.mask(invalidICDv9);

    assertEquals("Test ICDv9", maskedICDv9);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidICDv9InputInvalidHandlingReturnNull() throws Exception {
    ICDv9MaskingProviderConfig configuration = new ICDv9MaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider =
        new ICDv9MaskingProvider(configuration, tenantId, localizationProperty);

    String invalidICDv9 = "Invalid ICDv9";
    String maskedICDv9 = maskingProvider.mask(invalidICDv9);

    assertEquals(null, maskedICDv9);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

}
