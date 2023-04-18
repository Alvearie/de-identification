/*
 * © Merative US L.P. 2016,2020
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
import com.ibm.whc.deid.providers.identifiers.MACAddressIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.MACAddressMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;

public class MACAddressMaskingProviderTest extends TestLogSetUp {
  /*
   * Tests for preserve vendor option and its boolean values (true and false). It also tests for an
   * invalid value.
   */
  @Test
  public void testMask() throws Exception {
    // By default, the preserve vendor option is set to true.
    MACAddressMaskingProvider maskingProvider = new MACAddressMaskingProvider();
    MACAddressIdentifier identifier = new MACAddressIdentifier();

    String originalValue = "00:0a:95:9d:68:16";

    String maskedValue = maskingProvider.mask(originalValue);
    System.out.println(maskedValue);

    assertTrue(identifier.isOfThisType(maskedValue));
    assertFalse(maskedValue.equals(originalValue));
    assertTrue(maskedValue.toLowerCase().startsWith("00:0a:95:"));
  }

  @Test
  public void testMaskNoVendorPreservation() throws Exception {
    MACAddressMaskingProviderConfig configuration = new MACAddressMaskingProviderConfig();
    configuration.setMaskingPreserveVendor(false);
    MACAddressMaskingProvider maskingProvider = new MACAddressMaskingProvider(configuration);
    MACAddressIdentifier identifier = new MACAddressIdentifier();

    String originalValue = "00:0a:95:9d:68:16";

    String maskedValue = maskingProvider.mask(originalValue);
    assertTrue(identifier.isOfThisType(maskedValue));
    assertFalse(maskedValue.equals(originalValue));
    assertFalse(maskedValue.toLowerCase().startsWith("00:0a:95:"));
  }

  @Test
  public void testMaskNullMACAddressInputReturnNull() throws Exception {
    MACAddressMaskingProviderConfig configuration = new MACAddressMaskingProviderConfig();
    MaskingProvider maskingProvider = new MACAddressMaskingProvider(configuration);

    String invalidMACAddress = null;
    String maskedMACAddress = maskingProvider.mask(invalidMACAddress);

    assertEquals(null, maskedMACAddress);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidMACAddressInputValidHandlingReturnNull() throws Exception {
    MACAddressMaskingProviderConfig configuration = new MACAddressMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.NULL);
    MaskingProvider maskingProvider = new MACAddressMaskingProvider(configuration);

    String invalidMACAddress = "Invalid MAC Address";
    String maskedMACAddress = maskingProvider.mask(invalidMACAddress);

    assertEquals(null, maskedMACAddress);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidMACAddressInputValidHandlingReturnRandom() throws Exception {
    MACAddressMaskingProviderConfig configuration = new MACAddressMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    MaskingProvider maskingProvider = new MACAddressMaskingProvider(configuration);
    Identifier identifier = new MACAddressIdentifier();

    String invalidMACAddress = "Invalid MAC Address";
    String maskedMACAddress = maskingProvider.mask(invalidMACAddress);

    assertFalse(maskedMACAddress.equals(invalidMACAddress));
    assertTrue(identifier.isOfThisType(maskedMACAddress));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidMACAddressInputValidHandlingReturnDefaultCustomValue()
      throws Exception {
    MACAddressMaskingProviderConfig configuration = new MACAddressMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    MaskingProvider maskingProvider = new MACAddressMaskingProvider(configuration);
    String invalidMACAddress = "Invalid MAC Address";
    String maskedMACAddress = maskingProvider.mask(invalidMACAddress);

    assertEquals("OTHER", maskedMACAddress);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidMACAddressInputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    MACAddressMaskingProviderConfig configuration = new MACAddressMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    configuration.setUnexpectedInputReturnMessage("Test MAC Address");
    MaskingProvider maskingProvider = new MACAddressMaskingProvider(configuration);

    String invalidMACAddress = "Invalid MAC Address";
    String maskedMACAddress = maskingProvider.mask(invalidMACAddress);

    assertEquals("Test MAC Address", maskedMACAddress);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  @Ignore
  public void testPerformance() {
    int N = 1000000;
    MACAddressMaskingProviderConfig defaultConfiguration = new MACAddressMaskingProviderConfig();
    MACAddressMaskingProviderConfig nopreserveConfiguration = new MACAddressMaskingProviderConfig();
    nopreserveConfiguration.setMaskingPreserveVendor(false);

    MACAddressMaskingProviderConfig[] configurations =
        new MACAddressMaskingProviderConfig[] {defaultConfiguration, nopreserveConfiguration};

    String[] originalValues = new String[] {"00:0a:95:9d:68:16"};

    for (MACAddressMaskingProviderConfig maskingConfiguration : configurations) {
      MACAddressMaskingProvider maskingProvider =
          new MACAddressMaskingProvider(maskingConfiguration);

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
