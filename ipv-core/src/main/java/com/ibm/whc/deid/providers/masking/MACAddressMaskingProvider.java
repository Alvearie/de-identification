/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import com.ibm.whc.deid.providers.identifiers.MACAddressIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.MACAddressMaskingProviderConfig;

public class MACAddressMaskingProvider extends AbstractMaskingProvider {
  
  private static final long serialVersionUID = -5049095866017699568L;

  private static final char[] allowedCharacters = "abcdef0123456789".toCharArray();
  private static final MACAddressIdentifier macAddressIdentifier = new MACAddressIdentifier();
  
  private final boolean preserveVendor;
  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;

  /** Instantiates a new Mac address masking provider. */
  public MACAddressMaskingProvider() {
    this(new MACAddressMaskingProviderConfig());
  }

  /**
   * Instantiates a new Mac address masking provider.
   *
   * @param configuration the configuration
   */
  public MACAddressMaskingProvider(MACAddressMaskingProviderConfig configuration) {
    this.random = new SecureRandom();
    this.preserveVendor = configuration.isMaskingPreserveVendor();
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  private String randomMACAddress(int octets) {
    int subsetLength = allowedCharacters.length;

    StringBuilder builder = new StringBuilder((octets - 1) * 3 + 2);
    for (int i = 0; i < (octets - 1); i++) {
      builder.append(allowedCharacters[random.nextInt(subsetLength)]);
      builder.append(allowedCharacters[random.nextInt(subsetLength)]);
      builder.append(':');
    }

    builder.append(allowedCharacters[random.nextInt(subsetLength)]);
    builder.append(allowedCharacters[random.nextInt(subsetLength)]);

    return builder.toString();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    if (!preserveVendor) {
      return randomMACAddress(6);
    }

    if (!macAddressIdentifier.isOfThisType(identifier)) {
      debugFaultyInput("macAddressIdentifier");
      if (unspecifiedValueHandling == 2) {
        return randomMACAddress(6);
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    return identifier.substring(0, 9) + randomMACAddress(3);
  }
}
