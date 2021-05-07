/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;

import org.apache.commons.lang3.StringUtils;

import com.ibm.whc.deid.providers.identifiers.IPAddressIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.IPAddressMaskingProviderConfig;

public class IPAddressMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 6044504090581893891L;

  private static final IPAddressIdentifier ipAddressIdentifier = new IPAddressIdentifier();
  private final int preservedPrefixes;

  /**
   * Instantiates a new Ip address masking provider.
   *
   * @param configuration the configuration
   */
  public IPAddressMaskingProvider() {
    this(new IPAddressMaskingProviderConfig());
  }

  public IPAddressMaskingProvider(IPAddressMaskingProviderConfig configuration) {
    super(configuration);
    this.random = new SecureRandom();
    this.preservedPrefixes = configuration.getSubnetsPreserve();
  }

  private String randomSubnet() {
    int subnetAsInt = random.nextInt(256);
    return Integer.toString(subnetAsInt);
  }

  private String ipv4mask(String identifier) {
    String[] parts = identifier.split("\\.");
    String[] maskedParts = new String[4];

    if (this.preservedPrefixes >= 4) {
      return identifier;
    }

    for (int i = 0; i < parts.length; i++) {
      if (i < this.preservedPrefixes) {
        maskedParts[i] = parts[i];
      } else {
        maskedParts[i] = randomSubnet();
      }
    }

    return StringUtils.join(maskedParts, '.');
  }

  private String ipv6mask(String identifier) {
    return "::" + ipv4mask("1.2.3.4");
  }

  /**
   * Direct mask string.
   *
   * @param identifier the identifier
   * @param isIPv4 the is i pv 4
   * @return the string
   */
  /* TODO: find a way to protect this */
  public String directMask(String identifier, boolean isIPv4) {
    if (isIPv4) {
      return ipv4mask(identifier);
    }

    return ipv6mask(identifier);
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    if (ipAddressIdentifier.isIPv4(identifier)) {
      return directMask(identifier, true);
    } else if (ipAddressIdentifier.isIPv6(identifier)) {
      return directMask(identifier, false);
    } else {
      return applyUnexpectedValueHandling(identifier, () -> String.format("%d.%d.%d.%d",
          random.nextInt(255), random.nextInt(255), random.nextInt(255), random.nextInt(255)));
    }
  }
}
