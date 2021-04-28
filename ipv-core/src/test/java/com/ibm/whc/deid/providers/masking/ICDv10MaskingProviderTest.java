/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.models.ICD;
import com.ibm.whc.deid.models.ICDFormat;
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

  @Test
  public void testMaskConvertToCategory() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    MaskingProvider maskingProvider =
        new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);

    // default configuration is to convert to category
    String originalICD = "A01.0";
    String maskedICD = maskingProvider.mask(originalICD);
    assertEquals("A00-A09", maskedICD);

    originalICD = "Typhoid Fever";
    maskedICD = maskingProvider.mask(originalICD);
    assertEquals("Intestinal infectious diseases", maskedICD);

    originalICD = "not found";
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
    chapterConfiguration.setGeneralizeToChapter(true);

    ICDv10MaskingProviderConfig[] configurations =
        new ICDv10MaskingProviderConfig[] {defaultMaskingConfiguration, chapterConfiguration};

    for (ICDv10MaskingProviderConfig configuration : configurations) {

      MaskingProvider maskingProvider =
          new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);

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
        new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);

    String originalICD = "A01.0";
    String maskedICD = maskingProvider.mask(originalICD);
    assertFalse(originalICD.equals(maskedICD));
    assertTrue(maskedICD.equals("A00-B99"));

    originalICD = "typhoid Fever";
    maskedICD = maskingProvider.mask(originalICD);
    assertFalse(originalICD.equals(maskedICD));
    assertTrue(maskedICD.equals("Certain infectious and parasitic diseases"));

    originalICD = "not found";
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
        new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);

    String originalICD = "a04.0";
    for (int i = 0; i < 20; i++) {
      String maskedICD = maskingProvider.mask(originalICD);
      assertNotNull(maskedICD);
      ICD icd = maskingProvider.getManager().lookupICD(maskedICD);
      assertNotNull(icd);
      assertEquals(ICDFormat.CODE, icd.getFormat());
    }

    originalICD = "enteropathogenic escherichia cOLI infection";
    for (int i = 0; i < 20; i++) {
      String maskedICD = maskingProvider.mask(originalICD);
      assertNotNull(maskedICD);
      ICD icd = maskingProvider.getManager().lookupICD(maskedICD);
      assertNotNull(icd);
      assertEquals(ICDFormat.NAME, icd.getFormat());
    }

    // original not required to be recognized - replace with a code
    originalICD = "not found";
    for (int i = 0; i < 20; i++) {
      String maskedICD = maskingProvider.mask(originalICD);
      assertNotNull(maskedICD);
      ICD icd = maskingProvider.getManager().lookupICD(maskedICD);
      assertNotNull(icd);
      assertEquals(ICDFormat.CODE, icd.getFormat());
    }
  }

  @Test
  public void testMaskInvalidICDv10InputValidHandlingReturnNull() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    MaskingProvider maskingProvider =
        new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);

    String invalidICDv10 = "Invalid ICDv10";
    assertNull(maskingProvider.mask(invalidICDv10));
  }

  @Test
  public void testMaskInvalidICDv10InputValidHandlingReturnRandom() throws Exception {
    ICDv10MaskingProviderConfig configuration = new ICDv10MaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    ICDv10MaskingProvider maskingProvider =
        new ICDv10MaskingProvider(configuration, tenantId, localizationProperty);

    String invalidICDv10 = "Invalid ICDv10";
    String maskedICDv10 = maskingProvider.mask(invalidICDv10);
    assertNotNull(maskedICDv10);
    ICD icd = maskingProvider.getManager().lookupICD(maskedICDv10);
    assertNotNull(icd);
    assertEquals(ICDFormat.CODE, icd.getFormat());
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
      assertTrue(e.getMessage(), e.getMessage().contains("ICD code is missing"));
      assertTrue(e.getMessage(), e.getMessage().contains("Test X Name X"));
    }
  }
}
