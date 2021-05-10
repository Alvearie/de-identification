/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.Arrays;
import java.util.HashSet;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.pojo.config.masking.ICDv10MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
import com.ibm.whc.deid.utils.log.LogCodes;

public class ICDv10MaskingProviderTest implements MaskingProviderTest {

  /*
   * Tests for both generalize chapter and category options and their boolean values (true and
   * false). That is, tests converting the ICDv10 code or name to category or chapter. It also tests
   * for format, case , and invalid value.
   */

  public static final HashSet<String> TEST_ICDV10_DB_CODES =
      new HashSet<>(Arrays.asList("TESTA00.0", "TESTB00.0", "TESTC00.0", "TESTD00.0", "TESTE00.0"));
  public static final HashSet<String> TEST_ICDV10_DB_NAMES =
      new HashSet<>(Arrays.asList("Test v10 Name A", "Test v10 Name B", "Test v10 Name C",
          "Test v10 Name D", "Test v10 Name E"));

  // TESTA00.0,Test v10 Name A,A00-A09,Category Name A,A10-A19,Chapter Name A
  // TESTB00.0,Test v10 Name B,B00-B09,Category Name B,B10-B19,Chapter Name B
  // TESTC00.0,Test v10 Name C,C00-C09,Category Name C,C10-D19,Chapter Name C
  // TESTD00.0,Test v10 Name D,D00-D09,Category Name D,D10-D19,Chapter Name D
  // TESTE00.0,Test v10 Name E,E00-E09,Category Name E,C10-D19,Chapter Name E

  @Test
  public void testMaskConvertToCategory() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    MaskingProvider maskingProvider =
        new ICDv10MaskingProvider(configuration, tenantId, TEST_LOCALIZATION_PROPERTIES);

    // default configuration is to convert to category
    String originalICD = "TESTD00.0";
    String maskedICD = maskingProvider.mask(originalICD);
    assertEquals("D00-D09", maskedICD);

    originalICD = "Test v10 Name B";
    maskedICD = maskingProvider.mask(originalICD);
    assertEquals("Category Name B", maskedICD);

    originalICD = "A01.0";
    maskedICD = maskingProvider.mask(originalICD);
    assertEquals("OTHER", maskedICD);

    originalICD = "Typhoid Fever";
    maskedICD = maskingProvider.mask(originalICD);
    assertEquals("OTHER", maskedICD);
  }

  @Ignore
  @Test
  public void testPerformance() {
    ICDv10MaskingProviderConfig defaultMaskingConfiguration = new ICDv10MaskingProviderConfig();
    ICDv10MaskingProviderConfig chapterConfiguration = new ICDv10MaskingProviderConfig();

    defaultMaskingConfiguration.setGeneralizeToCategory(false);

    chapterConfiguration.setGeneralizeToCategory(false);
    chapterConfiguration.setGeneralizeToChapter(false);

    ICDv10MaskingProviderConfig[] configurations =
        new ICDv10MaskingProviderConfig[] {defaultMaskingConfiguration, chapterConfiguration};

    for (ICDv10MaskingProviderConfig configuration : configurations) {

      MaskingProvider maskingProvider =
          new ICDv10MaskingProvider(configuration, tenantId, TEST_LOCALIZATION_PROPERTIES);

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
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    configuration.setUnexpectedInputReturnMessage("msg1");
    configuration.setGeneralizeToCategory(false);
    configuration.setGeneralizeToChapter(true);
    MaskingProvider maskingProvider =
        new ICDv10MaskingProvider(configuration, tenantId, TEST_LOCALIZATION_PROPERTIES);

    String originalICD = "TESTC00.0";
    String maskedICD = maskingProvider.mask(originalICD);
    assertFalse(originalICD.equals(maskedICD));
    assertTrue(maskedICD, maskedICD.equals("C10-D19"));

    originalICD = "Test v10 Name A";
    maskedICD = maskingProvider.mask(originalICD);
    assertFalse(originalICD.equals(maskedICD));
    assertTrue(maskedICD.equals("Chapter Name A"));

    originalICD = "A01.0";
    maskedICD = maskingProvider.mask(originalICD);
    assertEquals("msg1", maskedICD);

    originalICD = "typhoid Fever";
    maskedICD = maskingProvider.mask(originalICD);
    assertEquals("msg1", maskedICD);
  }

  @Test
  public void testMaskConvertToRandom() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    configuration.setGeneralizeToCategory(false);
    configuration.setGeneralizeToChapter(false);
    ICDv10MaskingProvider maskingProvider =
        new ICDv10MaskingProvider(configuration, tenantId, TEST_LOCALIZATION_PROPERTIES);

    String originalICD = TEST_ICDV10_DB_CODES.iterator().next();
    boolean changed = false;
    for (int i = 0; i < 20; i++) {
      String maskedICD = maskingProvider.mask(originalICD);
      assertTrue(maskedICD, TEST_ICDV10_DB_CODES.contains(maskedICD));
      if (!originalICD.equalsIgnoreCase(maskedICD)) {
        changed = true;
      }
    }
    assertTrue(changed);

    originalICD = TEST_ICDV10_DB_NAMES.iterator().next();
    changed = false;
    for (int i = 0; i < 20; i++) {
      String maskedICD = maskingProvider.mask(originalICD);
      assertTrue(maskedICD, TEST_ICDV10_DB_NAMES.contains(maskedICD));
      if (!originalICD.equalsIgnoreCase(maskedICD)) {
        changed = true;
      }
    }
    assertTrue(changed);

    // original not required to be recognized - replace with a code
    originalICD = "not found";
    for (int i = 0; i < 20; i++) {
      String maskedICD = maskingProvider.mask(originalICD);
      assertTrue(maskedICD, TEST_ICDV10_DB_CODES.contains(maskedICD));
    }
  }

  @Test
  public void testMaskInvalidICDv10InputValidHandlingReturnNull() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    MaskingProvider maskingProvider =
        new ICDv10MaskingProvider(configuration, tenantId, TEST_LOCALIZATION_PROPERTIES);

    String invalidICDv10 = "Invalid ICDv10";
    assertNull(maskingProvider.mask(invalidICDv10));
  }

  @Test
  public void testMaskInvalidICDv10InputValidHandlingReturnRandom() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    ICDv10MaskingProvider maskingProvider =
        new ICDv10MaskingProvider(configuration, tenantId, TEST_LOCALIZATION_PROPERTIES);

    String invalidICDv10 = "Invalid ICDv10";
    for (int i = 0; i < 20; i++) {
      String maskedICDv10 = maskingProvider.mask(invalidICDv10);
      assertTrue(maskedICDv10, TEST_ICDV10_DB_CODES.contains(maskedICDv10));
    }
  }

  @Test
  public void testMaskInvalidICDv10InputValidHandlingReturnRandomEmpty() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    ICDv10MaskingProvider maskingProvider =
        new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);

    String invalidICDv10 = "Invalid ICDv10";
    String maskedICDv10 = maskingProvider.mask(invalidICDv10);
    assertNull(maskedICDv10);
  }

  @Test
  public void testLoadError() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    MaskingProvider maskingProvider =
        new ICDv10MaskingProvider(configuration, tenantId, ERROR_LOCALIZATION_PROPERTIES);

    // default configuration is to convert to category
    try {
      maskingProvider.mask("A01.0");
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertTrue(e.getMessage(), e.getMessage().contains("ICD code"));
      assertTrue(e.getMessage(), e.getMessage().contains("Test X Name X,"));
    }
  }
}
