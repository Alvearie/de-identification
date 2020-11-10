/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import java.text.StringCharacterIterator;
import com.ibm.whc.deid.configuration.MaskingConfiguration;
import com.ibm.whc.deid.shared.pojo.config.masking.CreditCardMaskingProviderConfig;
import com.ibm.whc.deid.util.RandomGenerators;

/**
 * The type Credit card masking provider.
 *
 */
public class CreditCardMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -5924474542829037368L;

  private final boolean preserveIssuer;
  private int preservedDigits = 0;
  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;

  /**
   * Instantiates a new Credit card masking provider.
   *
   * @param configuration the configuration
   */
  public CreditCardMaskingProvider(MaskingConfiguration configuration) {
    this(new SecureRandom(), configuration);
  }

  /**
   * Instantiates a new Credit card masking provider.
   *
   * @param random the random
   */
  public CreditCardMaskingProvider() {
    this(new CreditCardMaskingProviderConfig());
  }

  /**
   * Instantiates a new Credit card masking provider.
   *
   * @param random the random
   * @param configuration the configuration
   */
  public CreditCardMaskingProvider(SecureRandom random, MaskingConfiguration configuration) {
    this.random = random;
    this.unspecifiedValueHandling = configuration.getIntValue("unspecified.value.handling");
    this.unspecifiedValueReturnMessage =
        configuration.getStringValue("unspecified.value.returnMessage");

    this.preserveIssuer = (boolean) configuration.getValue("creditCard.issuer.preserve");
    if (preserveIssuer) {
      this.preservedDigits = 6;
    }
  }

  public CreditCardMaskingProvider(CreditCardMaskingProviderConfig configuration) {
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();

    this.preserveIssuer = configuration.isIssuerPreserve();
    if (preserveIssuer) {
      this.preservedDigits = 6;
    }
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    if (!preserveIssuer) {
      return RandomGenerators.generateRandomCreditCard();
    }

    final StringBuilder buffer = new StringBuilder();
    new StringCharacterIterator(identifier);

    int digitsEncountered = 0;
    int identifierLength = identifier.length();
    for (int i = 0; i < identifierLength; i++) {
      char c = identifier.charAt(i);
      if (Character.isDigit(c)) {
        digitsEncountered++;
      }
    }

    if (digitsEncountered != 16) {
      debugFaultyInput("digitsEncountered");
      if (unspecifiedValueHandling == 2) {
        return RandomGenerators.generateRandomCreditCard();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }
    digitsEncountered = 0;

    for (int i = 0; i < identifierLength; i++) {
      char c = identifier.charAt(i);

      if (Character.isDigit(c)) {
        digitsEncountered++;

        if (digitsEncountered > preservedDigits) {
          if (i < (identifierLength - 1)) {
            c = RandomGenerators.randomDigit();
          } else {
            c = (char) ('0'
                + RandomGenerators.luhnCheckDigit(buffer.toString().replaceAll("[\\- ]", "")));
          }
        }
      }

      buffer.append(c);
    }

    return buffer.toString();
  }
}
