/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import org.iban4j.CountryCode;
import org.iban4j.Iban;
import com.ibm.whc.deid.providers.identifiers.IBANIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.IBANMaskingProviderConfig;

public class IBANMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 107169194604514108L;

  private static final IBANIdentifier ibanIdentifier = new IBANIdentifier();

  private final boolean preserveCountry;
  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;

  /** Instantiates a new Iban masking provider. */
  public IBANMaskingProvider() {
    this(new IBANMaskingProviderConfig());
  }

  /**
   * Instantiates a new Iban masking provider.
   *
   * @param random the random
   * @param configuration the configuration
   */
  public IBANMaskingProvider(IBANMaskingProviderConfig configuration) {
    this.random = new SecureRandom();
    this.preserveCountry = configuration.isMaskPreserveCountry();
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    if (!ibanIdentifier.isOfThisType(identifier)) {
      debugFaultyInput("ibanIdentifier");
      if (unspecifiedValueHandling == 2) {
        return Iban.random().toString();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    if (this.preserveCountry) {
      return Iban.random(CountryCode.valueOf(identifier.substring(0, 2))).toString();
    }

    return Iban.random().toString();
  }
}
