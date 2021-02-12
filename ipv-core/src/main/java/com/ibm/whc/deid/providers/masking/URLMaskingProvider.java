/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.net.MalformedURLException;
import java.net.URL;
import java.security.SecureRandom;
import org.apache.commons.lang3.StringUtils;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.RedactMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.URLMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.util.RandomGenerators;

public class URLMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 5992875957296289762L;

  private final boolean maskUsernamePassword;
  private final boolean randomizePort;
  private final boolean removeQuery;
  private final boolean maskQuery;
  private final int preserveDomains;
  private transient final MaskingProviderFactory maskingProviderFactory =
      MaskingProviderFactoryUtil.getMaskingProviderFactory();
  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;
  private final String tenantId;
  private final DeidMaskingConfig deidMaskingConfig;


  public URLMaskingProvider(URLMaskingProviderConfig configuration, String tenantId,
      DeidMaskingConfig deidMaskingConfig) {
    this.random = new SecureRandom();
    this.maskUsernamePassword = configuration.isMaskUsernamePassword();
    this.randomizePort = configuration.isMaskPort();
    this.preserveDomains = configuration.getPreserveDomains();
    this.removeQuery = configuration.isMaskRemoveQuery();
    this.maskQuery = configuration.isMaskMaskQuery();
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
    this.tenantId = tenantId;
    this.deidMaskingConfig = deidMaskingConfig;
  }

  private int randomizePort(int exceptionPort) {
    int ret = random.nextInt(65536);
    return (ret == exceptionPort) ? randomizePort(exceptionPort) : ret;
  }

  private String randomizeUsernamePassword(String userInfo) {
    String username = null;
    String password = null;

    String[] parts = userInfo.split(":");

    username = RandomGenerators.randomUIDGenerator(parts[0].length());

    if (parts.length == 2) {
      password = RandomGenerators.randomUIDGenerator(8 + random.nextInt(8));

      return username + ":" + password;
    }

    return username;
  }

  private String randomizeHostname(String hostname) {
    /*
     * host can be expressed as a host name or a literal IP address. If IPv6 literal address is
     * used, it should be enclosed in square brackets ('[' and ']'), as specified by RFC 2732;
     */
    if (hostname.charAt(0) == '[' && hostname.charAt(hostname.length() - 1) == ']') {
      hostname = hostname.substring(1, hostname.length() - 1);
      return String.format("[%s]", RandomGenerators.randomHostnameGenerator(hostname, 0));
    }

    return RandomGenerators.randomHostnameGenerator(hostname, preserveDomains);
  }

  /**
   * Gets the masking provider used to mask portions of the query string.
   *
   * @param type the type
   * @return the masking provider
   */
  protected MaskingProvider getQueryPartMaskingProvider() {
    RedactMaskingProviderConfig providerConfig = new RedactMaskingProviderConfig();
    providerConfig.setPreserveLength(false);
    return maskingProviderFactory.getProviderFromType(MaskingProviderType.REDACT, deidMaskingConfig,
        providerConfig, tenantId);
  }

  private String maskQuery(String query) {
    if (query.equals("/") || query.equals("/?")) {
      return query;
    }

    String[] tokens = query.split("&");
    String[] maskedTokens = new String[tokens.length];

    MaskingProvider maskingProvider = null;

    for (int i = 0; i < tokens.length; i++) {
      String token = tokens[i];
      if (i == 0) {
        token = token.substring(1);
      }

      String[] parts = token.split("=");
      if (parts.length == 1 || parts[1].equals("")) {
        maskedTokens[i] = token;
        continue;
      }

      if (maskingProvider == null) {
        maskingProvider = getQueryPartMaskingProvider();
      }

      parts[1] = maskingProvider.mask(parts[1]);

      maskedTokens[i] = parts[0] + "=" + parts[1];
    }

    return "?" + StringUtils.join(maskedTokens, '&');
  }

  private String buildURLString(String protocol, String host, int port, String file,
      String userInfo) {
    StringBuilder builder = new StringBuilder(protocol);
    builder.append("://");

    if (userInfo != null) {
      builder.append(userInfo);
      builder.append("@");
    }

    builder.append(host);
    builder.append(":");
    builder.append(port);
    // builder.append("/");
    if (!removeQuery) {
      if (!maskQuery) {
        builder.append(file);
      } else {
        builder.append(maskQuery(file));
      }
    }
    return builder.toString();
  }

  private String maskURL(URL url) {
    String protocol = url.getProtocol();
    int port = url.getPort();
    String host = url.getHost();
    String filename = url.getFile();

    if (port == -1) {
      port = url.getDefaultPort();
    }

    if (this.randomizePort) {
      port = randomizePort(port);
    }

    host = randomizeHostname(host);

    String userInfo = url.getUserInfo();
    if (userInfo != null && this.maskUsernamePassword) {
      userInfo = randomizeUsernamePassword(userInfo);
    }

    return buildURLString(protocol, host, port, filename, userInfo);
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    URL url = null;

    try {
      url = new URL(identifier);
    } catch (MalformedURLException e) {
      // For this provider, we do not return random URL
      debugFaultyInput("url");
      return unspecifiedValueHandling == 3 ? unspecifiedValueReturnMessage : null;
    }

    return maskURL(url);
  }
}
