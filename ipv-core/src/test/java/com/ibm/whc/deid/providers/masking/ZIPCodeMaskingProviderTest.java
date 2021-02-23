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
import java.util.Arrays;
import java.util.List;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.ZIPCodeIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.ZIPCodeMaskingProviderConfig;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class ZIPCodeMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  private static final int MAX_LOOP_FOR_RANDOM_CHECK = 100;

	private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

  @Test
  public void testDoNothing() {
    ZIPCodeMaskingProviderConfig configuration = new ZIPCodeMaskingProviderConfig();
    configuration.setMaskSuffixTruncate(false);
    MaskingProvider maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    String zipcode = "00601";
    String maskedZipCode = maskingProvider.mask(zipcode);

    assertEquals(zipcode, maskedZipCode);
  }

  @Test
  public void testSuffixTruncate() {
    ZIPCodeMaskingProviderConfig configuration = new ZIPCodeMaskingProviderConfig();
    MaskingProvider maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    String zipcode = "00601";
    String maskedZipCode = maskingProvider.mask(zipcode);

    assertEquals("006", maskedZipCode);
  }

  @Test
  public void testSuffixReplaceWithRandom() {
    ZIPCodeMaskingProviderConfig configuration = new ZIPCodeMaskingProviderConfig();
    configuration.setMaskSuffixTruncate(false);
    configuration.setMaskSuffixReplaceWithRandom(true);
    MaskingProvider maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    String zipcode = "00601";

    int randomOK = 0;
    for (int i = 0; i < MAX_LOOP_FOR_RANDOM_CHECK; ++i) {
      String maskedZipCode = maskingProvider.mask(zipcode);
      assertTrue(maskedZipCode.startsWith("006"));
      if (!maskedZipCode.equals(zipcode)) {
        randomOK++;
      }
    }

    assertTrue(randomOK > 0);
  }

  @Test
  public void testSuffixReplaceWithRandomValid() {
    ZIPCodeMaskingProviderConfig configuration = new ZIPCodeMaskingProviderConfig();
    configuration.setMaskPrefixLength(4);
    configuration.setMaskSuffixTruncate(false);
    configuration.setMaskSuffixReplaceWithRandom(true);
    configuration.setMaskSuffixReplaceWithValidOnly(true);
    MaskingProvider maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    // ZIP codes with prefix 0060 (based on zcta.csv)
    List<String> validZipCodes = Arrays.asList("00601", "00602", "00603", "00606");
    String zipcode = "00601";

    int randomOK = 0;
    for (int i = 0; i < MAX_LOOP_FOR_RANDOM_CHECK; ++i) {
      String maskedZipCode = maskingProvider.mask(zipcode);
      assertTrue(validZipCodes.contains(maskedZipCode));
      if (!maskedZipCode.equals(zipcode)) {
        randomOK++;
      }
    }

    assertTrue(randomOK > 0);
  }

  @Test
  public void testMinimumPopulationWithinPrefixReachMinimum() {
    ZIPCodeMaskingProviderConfig configuration = new ZIPCodeMaskingProviderConfig();
    configuration.setMaskPrefixRequireMinPopulation(true);
    configuration.setMaskPrefixMinPopulation(1214568);
    MaskingProvider maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    String zipcode = "00601"; // Total population of ZIP codes with prefix
    // 006 is 1214568 (based on zcta.csv)
    String maskedZipCode = maskingProvider.mask(zipcode);

    assertEquals("006", maskedZipCode);
  }

  @Test
  public void testMinimumPopulationWithPrefixNotReachMinimum() {
    ZIPCodeMaskingProviderConfig configuration = new ZIPCodeMaskingProviderConfig();
    configuration.setMaskPrefixRequireMinPopulation(true);
    configuration.setMaskPrefixMinPopulation(1214569);
    MaskingProvider maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    String zipcode = "00601"; // Total population of ZIP codes with prefix
    // 006 is 1214568 (based on zcta.csv)
    String maskedZipCode = maskingProvider.mask(zipcode);

    assertEquals("000", maskedZipCode);

    //
    // Update to not truncate suffix
    //
    configuration.setMaskSuffixTruncate(false);
    maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);
    maskedZipCode = maskingProvider.mask(zipcode);

    assertEquals("00001", maskedZipCode);

    //
    // Update prefix to be bigger than specified ZIP code
    //
    configuration.setMaskPrefixLength(6);
    maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    maskedZipCode = maskingProvider.mask(zipcode);

    assertEquals("000000", maskedZipCode);
  }

  @Test
  public void testMinimumPopulationWithPrefixNotReachMinimumTruncate() {
    ZIPCodeMaskingProviderConfig configuration = new ZIPCodeMaskingProviderConfig();
    configuration.setMaskPrefixRequireMinPopulation(true);
    configuration.setMaskPrefixMinPopulation(1214569);
    configuration.setMaskTruncateIfNotMinPopulation(true);
    configuration.setMaskTruncateLengthIfNotMinPopulation(2);
    MaskingProvider maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    String zipcode = "00601"; // Total population of ZIP codes with prefix
    // 006 is 1214568 (based on zcta.csv)
    String maskedZipCode = maskingProvider.mask(zipcode);

    assertEquals("00", maskedZipCode);

    //
    // Update prefix to be bigger than specified ZIP code
    //
    configuration.setMaskPrefixLength(6);
    maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    maskedZipCode = maskingProvider.mask(zipcode);

    assertEquals("00", maskedZipCode);
  }

  @Test
  public void testReplaceWithNeighbor() throws Exception {
    ZIPCodeMaskingProviderConfig configuration = new ZIPCodeMaskingProviderConfig();
    configuration.setMaskSuffixTruncate(false);
    configuration.setMaskReplaceWithNeighbor(true);
    configuration.setMaskReplaceWithNeighborNearestCount(4);
    MaskingProvider maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    String zipcode = "85123";
    // Nearest neighbors (based on POSTAL_CODES.csv)
    List<String> nearestNeighbors =
        Arrays.asList("85123", "85131", "85241", "85130", "85141", "85194");

    int randomOK = 0;
    for (int i = 0; i < MAX_LOOP_FOR_RANDOM_CHECK; ++i) {
      String maskedZipCode = maskingProvider.mask(zipcode);
      assertTrue(nearestNeighbors.contains(maskedZipCode));
      if (!maskedZipCode.equals(zipcode)) {
        randomOK++;
      }
    }

    assertTrue(randomOK > 0);

    //
    // Update replaceWithNeighborNearestCount to smaller value
    //
    configuration.setMaskReplaceWithNeighborNearestCount(1);
    maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    // Nearest neighbors (based on POSTAL_CODES.csv)
    nearestNeighbors = Arrays.asList("85123", "85131", "85241");

    randomOK = 0;
    for (int i = 0; i < MAX_LOOP_FOR_RANDOM_CHECK; ++i) {
      String maskedZipCode = maskingProvider.mask(zipcode);
      assertTrue(nearestNeighbors.contains(maskedZipCode));
      if (!maskedZipCode.equals(zipcode)) {
        randomOK++;
      }
    }

    assertTrue(randomOK > 0);

    //
    // Update replaceWithNeighborNearestCount to 0
    //
    configuration.setMaskReplaceWithNeighborNearestCount(0);
    maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    String maskedZipCode = maskingProvider.mask(zipcode);

    assertEquals(zipcode, maskedZipCode);
  }

  @Test
  public void testMaskNullZIPCodeInputReturnNull() throws Exception {
    ZIPCodeMaskingProviderConfig configuration = new ZIPCodeMaskingProviderConfig();
    MaskingProvider maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidZIPCode = null;
    String maskedZIPCode = maskingProvider.mask(invalidZIPCode);

    assertEquals(null, maskedZIPCode);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidZIPCodeInputReturnNull() throws Exception {
    ZIPCodeMaskingProviderConfig configuration = new ZIPCodeMaskingProviderConfig();
    MaskingProvider maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidZIPCode = "Invalid ZIPCode";
    String maskedZIPCode = maskingProvider.mask(invalidZIPCode);

    assertEquals(null, maskedZIPCode);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskValidZIPCodeInputInvalidLengthValidHandlingReturnNull() throws Exception {
    ZIPCodeMaskingProviderConfig configuration = new ZIPCodeMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidZIPCode = "8512";
    String maskedZIPCode = maskingProvider.mask(invalidZIPCode);

    assertEquals(null, maskedZIPCode);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidZIPCodeInputValidHandlingReturnRandom() throws Exception {
    ZIPCodeMaskingProviderConfig configuration = new ZIPCodeMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);
    Identifier identifier = new ZIPCodeIdentifier();

    String invalidZIPCode = "8512";
    String maskedZIPCode = maskingProvider.mask(invalidZIPCode);

    assertFalse(maskedZIPCode.equals(invalidZIPCode));
    assertTrue(identifier.isOfThisType(maskedZIPCode));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidZIPCodeInputValidHandlingReturnDefaultCustomValue() throws Exception {
    ZIPCodeMaskingProviderConfig configuration = new ZIPCodeMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidZIPCode = "8512";
    String maskedZIPCode = maskingProvider.mask(invalidZIPCode);

    assertEquals("OTHER", maskedZIPCode);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidZIPCodeInputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    ZIPCodeMaskingProviderConfig configuration = new ZIPCodeMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("00000");
    MaskingProvider maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidZIPCode = "8512";
    String maskedZIPCode = maskingProvider.mask(invalidZIPCode);

    assertEquals("00000", maskedZIPCode);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidZIPCodeInputInvalidHandlingReturnNull() throws Exception {
    ZIPCodeMaskingProviderConfig configuration = new ZIPCodeMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new ZIPCodeMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidZIPCode = "8512";
    String maskedZIPCode = maskingProvider.mask(invalidZIPCode);

    assertEquals(null, maskedZIPCode);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

}
