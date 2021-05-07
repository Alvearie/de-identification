/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.ibm.whc.deid.models.County;
import com.ibm.whc.deid.providers.identifiers.CountyIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.CountyMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.util.CountyManager;

public class CountyMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  /*
   * Test for pseudorandom boolean option (true and false).
   */
  @Test
  public void testPseudorandom() throws Exception {
    CountyMaskingProviderConfig configuration = new CountyMaskingProviderConfig();
    assertEquals(MaskingProviderType.COUNTY, configuration.getType());
    configuration.setMaskPseudorandom(true);

    MaskingProvider maskingProvider =
        new CountyMaskingProvider(configuration, tenantId, localizationProperty);

    String original = "NotFoundXXXX";
    String masked = maskingProvider.mask(original);
    String firstMask = masked;
    for (int i = 0; i < 10; i++) {
      masked = maskingProvider.mask(original);
      assertEquals(firstMask, masked);
    }

    original = "Olmsted";
    masked = maskingProvider.mask(original);
    firstMask = masked;
    for (int i = 0; i < 10; i++) {
      masked = maskingProvider.mask(original);
      assertEquals(firstMask, masked);
    }
  }

  @Test
  public void testMask() {
    // maskPseudorandom by default is off
    CountyIdentifier identifier = new CountyIdentifier(tenantId, localizationProperty);
    CountyMaskingProviderConfig configuration = new CountyMaskingProviderConfig();
    assertEquals(MaskingProviderType.COUNTY, configuration.getType());

    CountyMaskingProvider maskingProvider =
        new CountyMaskingProvider(configuration, tenantId, localizationProperty);

    String originalValue = "Pendleton County";
    assertTrue(identifier.isOfThisType(originalValue));

    int randomizationOK = 0;
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(originalValue);
      assertTrue(maskedValue, identifier.isOfThisType(maskedValue));
      if (!maskedValue.equals(originalValue)) {
        randomizationOK++;
      }
    }
    assertTrue(randomizationOK > 0);

    // try short name
    originalValue = "Olmsted";
    assertTrue(identifier.isOfThisType(originalValue));

    randomizationOK = 0;
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(originalValue);
      assertTrue(identifier.isOfThisType(maskedValue));
      if (!maskedValue.equals(originalValue)) {
        randomizationOK++;
      }
    }
    assertTrue(randomizationOK > 0);
  }

  @Test
  public void testMask_FullOrShort() {
    CountyMaskingProviderConfig configuration = new CountyMaskingProviderConfig();
    assertEquals(MaskingProviderType.COUNTY, configuration.getType());

    // Use the Test localization file for this test because the list of counties has no collisions
    // between full and short names, whereas the production file does

    CountyMaskingProvider maskingProvider =
        new CountyMaskingProvider(configuration, tenantId, TEST_LOCALIZATION_PROPERTIES);
    CountyManager countyManager = maskingProvider.getCountyManager();

    // full name
    String originalValue = "County 3";

    int randomizationOK = 0;
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(originalValue);
      if (!maskedValue.equals(originalValue)) {
        randomizationOK++;
      }
      County county = countyManager.getValue(maskedValue);
      assertNotNull(county);
      assertTrue(county.getKey(), county.isUseFullNameAsKey());
    }
    assertTrue(randomizationOK > 0);

    // short name
    originalValue = "County2";

    randomizationOK = 0;
    for (int i = 0; i < 100; i++) {
      String maskedValue = maskingProvider.mask(originalValue);
      if (!maskedValue.equals(originalValue)) {
        randomizationOK++;
      }
      County county = countyManager.getValue(maskedValue);
      assertNotNull(county);
      assertFalse(county.isUseFullNameAsKey());
    }
    assertTrue(randomizationOK > 0);

    assertSame(countyManager, maskingProvider.getCountyManager());
  }
}
