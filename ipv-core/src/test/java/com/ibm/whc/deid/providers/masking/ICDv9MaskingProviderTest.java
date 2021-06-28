/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.models.ICD;
import com.ibm.whc.deid.models.ICDFormat;
import com.ibm.whc.deid.providers.identifiers.ICDv9Identifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.pojo.config.masking.ICDv10MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.ICDv9MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
import com.ibm.whc.deid.utils.log.LogCodes;

public class ICDv9MaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  /*
   * Tests for both generalize chapter and category options and their boolean values (true and
   * false). That is, tests converting the ICDv9 code or name to category or chapter. It also tests
   * for format, case , and invalid value.
   */

  @Test
  public void testMaskConvertToCategory() throws Exception {
    ICDv9MaskingProviderConfig configuration = new ICDv9MaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    MaskingProvider maskingProvider =
        new ICDv9MaskingProvider(configuration, tenantId, localizationProperty);

    // default configuration is to convert to category
    String originalICD = "002.0";
    String maskedICD = maskingProvider.mask(originalICD);
    assertEquals("002", maskedICD);

    originalICD = "typhoid FEVer";
    maskedICD = maskingProvider.mask(originalICD);
    assertEquals("Typhoid and paratyphoid fevers", maskedICD);

    originalICD = "not found";
    maskedICD = maskingProvider.mask(originalICD);
    assertEquals("OTHER", maskedICD);
  }

  @Test
  @Ignore
  public void testPerformance() {
    ICDv9MaskingProviderConfig defaultMaskingConfiguration = new ICDv9MaskingProviderConfig();
    ICDv9MaskingProviderConfig chapterConfiguration = new ICDv9MaskingProviderConfig();

    defaultMaskingConfiguration.setGeneralizeToCategory(false);

    chapterConfiguration.setGeneralizeToCategory(false);
    chapterConfiguration.setGeneralizeToChapter(true);

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
    configuration.setGeneralizeToCategory(false);
    configuration.setGeneralizeToChapter(true);
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    MaskingProvider maskingProvider =
        new ICDv9MaskingProvider(configuration, tenantId, localizationProperty);

    // Test convert to chapter
    String originalICD = "002.0";
    String maskedICD = maskingProvider.mask(originalICD);
    assertEquals("001-139", maskedICD);

    originalICD = "TypHOID Fever";
    maskedICD = maskingProvider.mask(originalICD);
    assertEquals("infectious and parasitic diseases", maskedICD);

    originalICD = "not found";
    maskedICD = maskingProvider.mask(originalICD);
    assertEquals("OTHER", maskedICD);
  }

  @Test
  public void testMaskConvertToRandom() throws Exception {
    ICDv9MaskingProviderConfig configuration = new ICDv9MaskingProviderConfig();
    configuration.setGeneralizeToCategory(false);
    configuration.setGeneralizeToChapter(false);
    ICDv9MaskingProvider maskingProvider =
        new ICDv9MaskingProvider(configuration, tenantId, localizationProperty);

    // by code
    String originalICD = "002.0";
    for (int i = 0; i < 20; i++) {
      String maskedICD = maskingProvider.mask(originalICD);
      assertNotNull(maskedICD);
      ICD icd = maskingProvider.getManager().lookupICD(maskedICD);
      assertNotNull(icd);
      assertEquals(ICDFormat.CODE, icd.getFormat());
    }

    // by name given full name
    originalICD = "SalmOnella GastroenteritIS";
    for (int i = 0; i < 20; i++) {
      String maskedICD = maskingProvider.mask(originalICD);
      assertNotNull(maskedICD);
      ICD icd = maskingProvider.getManager().lookupICD(maskedICD);
      assertNotNull(icd);
      assertEquals(ICDFormat.NAME, icd.getFormat());
    }

    // by name given short name
    originalICD = "salmonella eNTeritis";
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
  public void testMaskInvalidICDv9InputValidHandlingReturnRandom() throws Exception {
    ICDv9MaskingProviderConfig configuration = new ICDv9MaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    ICDv9MaskingProvider maskingProvider =
        new ICDv9MaskingProvider(configuration, tenantId, localizationProperty);
    Identifier identifier = new ICDv9Identifier(tenantId, localizationProperty);

    String invalidICDv9 = "Invalid ICDv9";
    String maskedICDv9 = maskingProvider.mask(invalidICDv9);

    assertNotNull(maskedICDv9);
    ICD icd = maskingProvider.getManager().lookupICD(maskedICDv9);
    assertNotNull(icd);
    assertEquals(ICDFormat.CODE, icd.getFormat());
    assertTrue(identifier.isOfThisType(maskedICDv9));
  }

  @Test
  public void testLoadError() throws Exception {
    ICDv9MaskingProviderConfig configuration = new ICDv9MaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    MaskingProvider maskingProvider =
        new ICDv9MaskingProvider(configuration, tenantId, ERROR_LOCALIZATION_PROPERTIES);

    // default configuration is to convert to category
    try {
      maskingProvider.mask("A01.0");
      fail("expected exception");
    } catch (KeyedRuntimeException e) {
      assertEquals(LogCodes.WPH1023E, e.getMessageKey());
      assertTrue(e.getMessage(), e.getMessage().contains(", X10-X19,"));
      assertTrue(e.getMessage(), e.getMessage().contains("ICD chapter code"));
    }
  }
}
