/*
 * (C) Copyright IBM Corp. 2016,2021
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
    super(configuration);
    this.random = new SecureRandom();
    this.preserveCountry = configuration.isMaskPreserveCountry();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    if (this.preserveCountry) {
      if (!ibanIdentifier.isOfThisType(identifier)) {
        return applyUnexpectedValueHandling(identifier, () -> Iban.random().toString());
      }
      return Iban.random(CountryCode.valueOf(identifier.substring(0, 2))).toString();
    }

    return Iban.random().toString();
  }
}
