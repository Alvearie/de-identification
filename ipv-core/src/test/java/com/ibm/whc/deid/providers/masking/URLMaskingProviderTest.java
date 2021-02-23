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
import static org.junit.Assert.assertTrue;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Objects;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.IPAddressIdentifier;
import com.ibm.whc.deid.providers.identifiers.URLIdentifier;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.URLMaskingProviderConfig;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class URLMaskingProviderTest extends TestLogSetUp {
	private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;
  URLIdentifier identifier = new URLIdentifier();

  String tenantId = "TEST_TENANT";

  DeidMaskingConfig deidMaskingConfig = new DeidMaskingConfig();
  /*
   * The test cases cover all the option flags for true and false values, and for the preserve
   * domain option the values of 0, 1 (default), 2 (sub-domain) and 5 (log domain) are tested.
   */

  public boolean URLsMatch(String u1, String u2) throws MalformedURLException {
    URL url1 = new URL(u1);
    URL url2 = new URL(u2);

    String userinfo1 = url1.getUserInfo();
    String userinfo2 = url2.getUserInfo();

    int port1 = url1.getPort();
    if (port1 == -1) {
      port1 = url1.getDefaultPort();
    }

    int port2 = url2.getPort();
    if (port2 == -1) {
      port2 = url2.getDefaultPort();
    }

    if (userinfo1 == null) {
      userinfo1 = "";
    }

    if (userinfo2 == null) {
      userinfo2 = "";
    }

    String file1 = url1.getFile();
    if (file1.equals("/")) {
      file1 = "";
    }

    String file2 = url2.getFile();
    if (file2.equals("/")) {
      file2 = "";
    }

    return url1.getHost().equals(url2.getHost()) && url1.getProtocol().equals(url2.getProtocol())
        && port1 == port2 && file1.equals(file2) && userinfo1.equals(userinfo2);
  }

  @Test
  public void testDefaultMask() throws Exception {
		URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
		MaskingProvider urlMaskingProvider = new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);

    String url = "http://www.nba.com";
    String maskedResult = urlMaskingProvider.mask(url);

    assertFalse(URLsMatch(url, maskedResult));
    assertTrue(identifier.isOfThisType(maskedResult));
  }

  @Test
  public void testURLWithIPv4Address() throws Exception {
		URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
		MaskingProvider urlMaskingProvider = new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);
    IPAddressIdentifier ipAddressIdentifier = new IPAddressIdentifier();

    String url = "http://10.22.33.44";
    String maskedResult = urlMaskingProvider.mask(url);

    assertFalse(URLsMatch(url, maskedResult));
    assertTrue(identifier.isOfThisType(maskedResult));
    String host = new URL(maskedResult).getHost();
    assertTrue(ipAddressIdentifier.isIPv4(host));
  }

  @Test
  public void testURLWithIPv6Address() throws Exception {
		URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
		MaskingProvider urlMaskingProvider = new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);
    IPAddressIdentifier ipAddressIdentifier = new IPAddressIdentifier();

    String url = "http://[1::4:5:6:7:8]:100/";
    String maskedResult = urlMaskingProvider.mask(url);
    assertTrue(identifier.isOfThisType(maskedResult));
    String host = new URL(maskedResult).getHost();
    assertTrue(host.charAt(0) == '[');
    assertTrue(host.charAt(host.length() - 1) == ']');
    assertTrue(ipAddressIdentifier.isIPv6(host.substring(1, host.length() - 1)));
  }

  @Test
  public void testNoDomainPreservation() throws Exception {
    URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
    configuration.setMaskUsernamePassword(true);
    configuration.setMaskPort(true);
    configuration.setPreserveDomains(0);
    MaskingProvider urlMaskingProvider =
        new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);

    // we do not preserve anything
    String url = "http://www.nba.com";
    int domainOK = 0;

    for (int i = 0; i < 100; i++) {
      String maskedResult = urlMaskingProvider.mask(url);
      assertTrue(identifier.isOfThisType(maskedResult));
      assertFalse(new URL(maskedResult).getHost().equals("www.nba.com"));
      assertFalse(new URL(maskedResult).getHost().startsWith("www.nba."));
      assertFalse(new URL(maskedResult).getHost().endsWith("nba.com"));
      if (!maskedResult.endsWith(".com")) {
        domainOK++;
      }
    }

    assertTrue(domainOK > 0);

    url = "http://www.nba.co.uk";
    domainOK = 0;
    for (int i = 0; i < 100; i++) {
      String maskedResult = urlMaskingProvider.mask(url);
      assertTrue(identifier.isOfThisType(maskedResult));
      assertFalse(new URL(maskedResult).getHost().equals("www.nba.co.uk"));
      assertFalse(new URL(maskedResult).getHost().startsWith("www.nba."));
      assertFalse(new URL(maskedResult).getHost().endsWith("nba.co.uk"));
      if (!maskedResult.endsWith(".co.uk")) {
        domainOK++;
      }
    }
    assertTrue(domainOK > 0);
  }

  @Test
  public void testTLDIsPreserverd() throws Exception {
    // we preserve TLD
    URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
    configuration.setMaskUsernamePassword(true);
    configuration.setMaskPort(true);
    configuration.setPreserveDomains(1);
    MaskingProvider urlMaskingProvider =
        new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);
    String url = "http://www.nba.com";
    String maskedResult = urlMaskingProvider.mask(url);
    assertTrue(identifier.isOfThisType(maskedResult));
    assertTrue(new URL(maskedResult).getHost().endsWith(".com"));

    url = "http://www.nba.co.uk";
    maskedResult = urlMaskingProvider.mask(url);
    assertTrue(identifier.isOfThisType(maskedResult));
    assertTrue(new URL(maskedResult).getHost().endsWith(".co.uk"));
  }

  @Test
  public void testFirstSubdomainIsPreserved() throws Exception {
    // we preserve TLD + first subdomain
    URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
    configuration.setMaskUsernamePassword(true);
    configuration.setMaskPort(true);
    configuration.setPreserveDomains(2);
    MaskingProvider urlMaskingProvider =
        new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);
    String url = "http://www.nba.com";
    String maskedResult = urlMaskingProvider.mask(url);
    assertTrue(identifier.isOfThisType(maskedResult));
    assertTrue(new URL(maskedResult).getHost().endsWith(".nba.com"));
    assertFalse(new URL(maskedResult).getHost().equals("www.nba.com"));
  }

  @Test
  public void testPreserveDomainValueLongerThanDomains() throws Exception {
    URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
    configuration.setMaskUsernamePassword(true);
    configuration.setMaskPort(true);
    configuration.setPreserveDomains(5);
    MaskingProvider urlMaskingProvider =
        new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);
    String url = "http://www.nba.com";
    String maskedResult = urlMaskingProvider.mask(url);
    assertTrue(identifier.isOfThisType(maskedResult));
    assertTrue(new URL(maskedResult).getHost().equals("www.nba.com"));
  }

  @Test
  public void testUsernamePasswordMask() throws Exception {

		URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
		MaskingProvider urlMaskingProvider = new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);

    /* we test that both username and passwords get randomized */
    String url = "http://user1:pass1@www.nba.com";
    String maskedResult = urlMaskingProvider.mask(url);
    assertTrue(identifier.isOfThisType(maskedResult));

    String originalUserInfo = new URL(url).getUserInfo();
    String maskedUserInfo = new URL(maskedResult).getUserInfo();
    assertFalse(Objects.equals(originalUserInfo, maskedUserInfo));
    assertFalse(maskedUserInfo == null);
    assertTrue(maskedUserInfo.split(":").length == 2);
    assertFalse(maskedUserInfo.split(":")[0].equals(originalUserInfo.split(":")[0]));
    assertFalse(maskedUserInfo.split(":")[1].equals(originalUserInfo.split(":")[1]));

    /* we test that username is randomized */
    url = "http://user1@www.nba.com";
    maskedResult = urlMaskingProvider.mask(url);
    assertTrue(identifier.isOfThisType(maskedResult));

    originalUserInfo = new URL(url).getUserInfo();
    maskedUserInfo = new URL(maskedResult).getUserInfo();
    assertTrue(maskedUserInfo.split(":").length == 1);
    assertFalse(maskedUserInfo.split(":")[0].equals(originalUserInfo.split(":")[0]));
  }

  @Test
  public void testUserInfoFlagOff() throws Exception {
    /*
     * we test that if we turn the flag off the user information is preserved
     */
    URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
    configuration.setMaskUsernamePassword(false);
    configuration.setMaskPort(true);
    configuration.setPreserveDomains(0);
    MaskingProvider urlMaskingProvider =
        new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);
    String url = "http://user1:pass1@www.nba.com";
    String maskedResult = urlMaskingProvider.mask(url);
    String originalUserInfo = new URL(url).getUserInfo();
    String maskedUserInfo = new URL(maskedResult).getUserInfo();
    assertTrue(originalUserInfo.equals(maskedUserInfo));
  }

  @Test
  public void testRandomizePortFlag() throws Exception {
    URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
    configuration.setMaskUsernamePassword(false);
    configuration.setMaskPort(true);
    configuration.setPreserveDomains(0);
    MaskingProvider urlMaskingProvider =
        new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);
    String url = "http://www.nba.com";
    String maskedResult = urlMaskingProvider.mask(url);
    URL maskedURL = new URL(maskedResult);
    int maskedPort = maskedURL.getPort();
    assertFalse(maskedPort == new URL(url).getDefaultPort());
    assertTrue(maskedPort > 0 && maskedPort < 65536);
  }

  @Test
  public void testRemoveQueryPart() throws Exception {
    URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
    configuration.setMaskRemoveQuery(true);
    MaskingProvider urlMaskingProvider =
        new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);
    String url = "http://www.nba.com?q=abcd";
    String maskedResult = urlMaskingProvider.mask(url);
    URL maskedURL = new URL(maskedResult);
    String maskedFile = maskedURL.getFile();
    assertTrue(maskedFile.equals("") || maskedFile.equals("/"));
  }

  @Test
  public void testMaskQueryPart() throws Exception {
    URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
    configuration.setMaskMaskQuery(true);
    MaskingProvider urlMaskingProvider =
        new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);
    String url = "http://www.nba.com?q=John&q2=Mary&q3=foobar&q4=&q5&q6=33&q7=true&state=Montana";
    String maskedResult = urlMaskingProvider.mask(url);
    URL maskedURL = new URL(maskedResult);
    String maskedFile = maskedURL.getFile();
    System.out.println(maskedResult);
    assertEquals("?q=X&q2=X&q3=X&q4=&q5&q6=X&q7=X&state=X", maskedFile);
  }

  @Test
  public void testMaskQueryEmpty() throws Exception {
    URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
    configuration.setMaskMaskQuery(true);
    MaskingProvider urlMaskingProvider =
        new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);
    String url = "http://www.nba.com/?";
    String maskedResult = urlMaskingProvider.mask(url);
    URL maskedURL = new URL(maskedResult);
    String maskedFile = maskedURL.getFile();
    System.out.println(maskedResult);
    assertTrue(maskedFile.equals("/?"));
  }

  @Test
  public void testMaskNullURLInputReturnNull() throws Exception {
    URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
    MaskingProvider maskingProvider =
        new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);

    String invalidURL = null;
    String maskedURL = maskingProvider.mask(invalidURL);

    assertEquals(null, maskedURL);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidURLInputValidHandlingReturnNull() throws Exception {
    URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);
    MaskingProvider maskingProvider =
        new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);

    String invalidURL = "Invalid URL";
    String maskedURL = maskingProvider.mask(invalidURL);

    assertEquals(null, maskedURL);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidURLInputValidHandlingReturnRandom() throws Exception {
    URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider =
        new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);

    String invalidURL = "Invalid URL";
    String maskedURL = maskingProvider.mask(invalidURL);

    // return null for this provider
    assertEquals(null, maskedURL);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidURLInputValidHandlingReturnDefaultCustomValue() throws Exception {
    URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider =
        new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);

    String invalidURL = "Invalid URL";
    String maskedURL = maskingProvider.mask(invalidURL);

    assertEquals("OTHER", maskedURL);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidURLInputValidHandlingReturnNonDefaultCustomValue() throws Exception {
    URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("Test URL");
    MaskingProvider maskingProvider =
        new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);

    String invalidURL = "Invalid URL";
    String maskedURL = maskingProvider.mask(invalidURL);

    assertEquals("Test URL", maskedURL);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidURLInputInvalidHandlingReturnNull() throws Exception {
    URLMaskingProviderConfig configuration = new URLMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider =
        new URLMaskingProvider(configuration, tenantId, deidMaskingConfig, localizationProperty);

    String invalidURL = "Invalid URL";
    String maskedURL = maskingProvider.mask(invalidURL);

    assertEquals(null, maskedURL);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  @Ignore
  public void testPerformance() {
    int N = 1000000;
    URLMaskingProviderConfig defaultConfiguration = new URLMaskingProviderConfig();

    URLMaskingProviderConfig[] configurations =
        new URLMaskingProviderConfig[] {defaultConfiguration};

    String[] originalValues =
        new String[] {"http://www.nba.com", "http://user1:pass1@www.nba.com", "http://10.22.33.44"};

    for (URLMaskingProviderConfig maskingConfiguration : configurations) {
      URLMaskingProvider maskingProvider =
          new URLMaskingProvider(maskingConfiguration, tenantId, deidMaskingConfig, localizationProperty);
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
