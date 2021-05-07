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
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.IPAddressIdentifier;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.shared.pojo.config.masking.IPAddressMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;

public class IPAddressMaskingProviderTest extends TestLogSetUp {

  /*
   * Tests for randomization of the IP address, masked address format, and IPV4 and IPV6 addresses
   * and formats. It also tests preservation of ipv4 andipv6 subnets.
   */

  @Test
  public void testIPv4Mask() throws Exception {
    MaskingProvider ccMaskingProvider = new IPAddressMaskingProvider();

    String originalIPv4Address = "122.133.10.198";
    String maskedResult = ccMaskingProvider.mask(originalIPv4Address);

    // basic test that we do not return the same
    assertFalse(originalIPv4Address.equals(maskedResult));

    // basic test to check that returned masked respects the format
    IPAddressIdentifier ipAddressIdentifier = new IPAddressIdentifier();
    assertTrue(ipAddressIdentifier.isOfThisType(maskedResult));

    // basic test to check that every time we randomize the result
    String maskedResult2 = ccMaskingProvider.mask(originalIPv4Address);
    assertFalse(maskedResult.equals(maskedResult2));
  }

  @Test
  public void testIPV4PreserveSubnets() throws Exception {
    IPAddressMaskingProviderConfig configuration = new IPAddressMaskingProviderConfig();
    configuration.setSubnetsPreserve(2);
    IPAddressMaskingProvider maskingProvider = new IPAddressMaskingProvider(configuration);

    String originalIPv4Address = "122.133.10.198";
    String maskedResult = maskingProvider.mask(originalIPv4Address);
    assertFalse(originalIPv4Address.equals(maskedResult));
    assertTrue(maskedResult.startsWith("122.133."));
  }

  @Test
  public void testIPV4PreserveFourSubnets() throws Exception {
    IPAddressMaskingProviderConfig configuration = new IPAddressMaskingProviderConfig();
    configuration.setSubnetsPreserve(4);
    IPAddressMaskingProvider maskingProvider = new IPAddressMaskingProvider(configuration);

    String originalIPv4Address = "122.133.10.198";
    String maskedResult = maskingProvider.mask(originalIPv4Address);
    assertTrue(maskedResult.startsWith("122.133.10.198"));
  }

  @Test
  public void testIPv6Mask() throws Exception {
    MaskingProvider ccMaskingProvider = new IPAddressMaskingProvider();

    String[] validIPv6Addresses = {"1:2:3:4:5:6:7:8", "1::", "1::8", "1::7:8", "1::6:7:8",
        "1::5:6:7:8", "1::4:5:6:7:8", "1::3:4:5:6:7:8", "fe80::7:8%eth0", "::255.255.255.255",
        "::ffff:255.255.255.255", "::FFFF:255.255.255.255", "::ffff:0:255.255.255.255",
        "::FFFF:0:255.255.255.255", "2001:db8:3:4::192.0.2.33", "64:ff9b::192.0.2.33"};

    IPAddressIdentifier ipAddressIdentifier = new IPAddressIdentifier();

    for (String ipv6address : validIPv6Addresses) {
      String maskedResult = ccMaskingProvider.mask(ipv6address);
      assertFalse(maskedResult.equals(ipv6address));
      assertTrue(ipAddressIdentifier.isIPv6(maskedResult));

      String maskedResult2 = ccMaskingProvider.mask(ipv6address);
      assertFalse(maskedResult.equals(maskedResult2));
    }
  }

  @Test
  @Ignore
  public void testPerformance() {
    int N = 1000000;
    IPAddressMaskingProviderConfig defaultConfiguration = new IPAddressMaskingProviderConfig();
    IPAddressMaskingProviderConfig preserveConfiguration = new IPAddressMaskingProviderConfig();
    preserveConfiguration.setSubnetsPreserve(2);

    IPAddressMaskingProviderConfig[] configurations =
        new IPAddressMaskingProviderConfig[] {defaultConfiguration, preserveConfiguration};

    String[] originalValues = new String[] {"122.133.10.198", "1:2:3:4:5:6:7:8"};

    for (IPAddressMaskingProviderConfig maskingConfiguration : configurations) {
      IPAddressMaskingProvider maskingProvider = new IPAddressMaskingProvider(maskingConfiguration);

      for (String originalValue : originalValues) {
        long startMillis = System.currentTimeMillis();

        for (int i = 0; i < N; i++) {
          maskingProvider.mask(originalValue);
        }

        long diff = System.currentTimeMillis() - startMillis;
        System.out.println(String.format(" %s: %d operations took %d milliseconds (%f per op)",
            originalValue, N, diff, (double) diff / N));
        // Assert test always should finish in less than 10 seconds
        assertTrue(diff < 10000);
      }
    }
  }

  @Test
  public void testMaskNullIPAddressInputReturnNull() throws Exception {
    IPAddressMaskingProviderConfig configuration = new IPAddressMaskingProviderConfig();
    MaskingProvider maskingProvider = new IPAddressMaskingProvider(configuration);

    String invalidIPAddress = null;
    String maskedIPAddress = maskingProvider.mask(invalidIPAddress);

    assertEquals(null, maskedIPAddress);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidIPAddressInputValidHandlingReturnNull() throws Exception {
    IPAddressMaskingProviderConfig configuration = new IPAddressMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider = new IPAddressMaskingProvider(configuration);

    String invalidIPAddress = "Invalid IP Address";
    String maskedIPAddress = maskingProvider.mask(invalidIPAddress);

    assertEquals(null, maskedIPAddress);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidIPAddressInputValidHandlingReturnRandom() throws Exception {
    IPAddressMaskingProviderConfig configuration = new IPAddressMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.RANDOM);
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new IPAddressMaskingProvider(configuration);
    Identifier identifier = new IPAddressIdentifier();

    String invalidIPAddress = "Invalid IP Address";
    String maskedIPAddress = maskingProvider.mask(invalidIPAddress);

    assertNotEquals(invalidIPAddress, maskedIPAddress);
    assertNotEquals("OTHER", maskedIPAddress);
    assertTrue(identifier.isOfThisType(maskedIPAddress));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidIPAddressInputValidHandlingReturnDefaultCustomValue()
      throws Exception {
    IPAddressMaskingProviderConfig configuration = new IPAddressMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider = new IPAddressMaskingProvider(configuration);

    String invalidIPAddress = "Invalid IP Address";
    String maskedIPAddress = maskingProvider.mask(invalidIPAddress);

    assertEquals("OTHER", maskedIPAddress);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidIPAddressInputValidHandlingReturnNonDefaultCustomValue()
      throws Exception {
    IPAddressMaskingProviderConfig configuration = new IPAddressMaskingProviderConfig();
    configuration.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    configuration.setUnexpectedInputReturnMessage("Test IP Address");
    configuration.setUnspecifiedValueHandling(2);
    configuration.setUnspecifiedValueReturnMessage("XTest IP Address");
    MaskingProvider maskingProvider = new IPAddressMaskingProvider(configuration);

    String invalidIPAddress = "Invalid IP Address";
    String maskedIPAddress = maskingProvider.mask(invalidIPAddress);

    assertEquals("Test IP Address", maskedIPAddress);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidIPAddressInputInvalidHandlingReturnNull() throws Exception {
    IPAddressMaskingProviderConfig configuration = new IPAddressMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider = new IPAddressMaskingProvider(configuration);

    String invalidIPAddress = "Invalid IP Address";
    String maskedIPAddress = maskingProvider.mask(invalidIPAddress);

    assertEquals(null, maskedIPAddress);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }
}
