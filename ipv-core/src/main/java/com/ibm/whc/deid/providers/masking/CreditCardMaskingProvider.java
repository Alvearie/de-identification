/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.CreditCardMaskingProviderConfig;
import com.ibm.whc.deid.util.CreditCardTypeManager;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.RandomGenerators;

/**
 * The type Credit card masking provider.
 *
 */
public class CreditCardMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -5924474542829037368L;

  private final boolean preserveIssuer;
  private final int preservedDigits;

  protected transient volatile CreditCardTypeManager creditCardTypeManager = null;

  public CreditCardMaskingProvider(CreditCardMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty, configuration);
    this.preserveIssuer = configuration.isIssuerPreserve();
    this.preservedDigits = this.preserveIssuer ? 6 : 0;
  }

  protected CreditCardTypeManager getCreditCardTypeManager() {
    if (creditCardTypeManager == null) {
      creditCardTypeManager = (CreditCardTypeManager) ManagerFactory.getInstance()
          .getManager(tenantId, Resource.CREDIT_CARD_TYPE, null, localizationProperty);
    }
    return creditCardTypeManager;
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    if (!preserveIssuer) {
      return RandomGenerators.generateRandomCreditCard(getCreditCardTypeManager());
    }

    final StringBuilder buffer = new StringBuilder();

    int digitsEncountered = 0;
    int identifierLength = identifier.length();
    for (int i = 0; i < identifierLength; i++) {
      char c = identifier.charAt(i);
      if (Character.isDigit(c)) {
        digitsEncountered++;
      }
    }

    if (digitsEncountered != 16) {
      return applyUnexpectedValueHandling(identifier,
          () -> RandomGenerators.generateRandomCreditCard(getCreditCardTypeManager()));
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
