/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.util.CreditCardTypeManager;

public class CreditCardTypeMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 3375383479009603851L;

  private final CreditCardTypeManager ccTypeManager;

  /**
   * Instantiates a new Credit card type masking provider.
   * 
   * @param tenantId TODO
   * @paramlocalizationProperty location of the localization property file
   */
  public CreditCardTypeMaskingProvider(String tenantId, String localizationProperty) {
    super(tenantId, localizationProperty);

    ccTypeManager = new CreditCardTypeManager(tenantId, localizationProperty);
  }

  @Override
  public String mask(String identifier) {
    return ccTypeManager.getRandomKey();
  }
}
