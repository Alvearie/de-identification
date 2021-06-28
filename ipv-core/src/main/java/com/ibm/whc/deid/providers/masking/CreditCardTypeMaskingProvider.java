/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.CreditCardType;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.CreditCardTypeManager;
import com.ibm.whc.deid.util.ManagerFactory;

public class CreditCardTypeMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 3375383479009603851L;

  protected transient volatile CreditCardTypeManager ccTypeManager = null;

  /**
   * Instantiates a new Credit card type masking provider.
   * 
   * @param tenantId the tenant associated with the current request
   * @paramlocalizationProperty location of the localization property file
   */
  public CreditCardTypeMaskingProvider(String tenantId, String localizationProperty) {
    super(tenantId, localizationProperty);
  }

  protected CreditCardTypeManager getCreditCardTypeManager() {
    if (ccTypeManager == null) {
      ccTypeManager = (CreditCardTypeManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.CREDIT_CARD_TYPE, null, localizationProperty);
    }
    return ccTypeManager;
  }

  @Override
  public String mask(String identifier) {
    CreditCardType ccType = getCreditCardTypeManager().getRandomValue();
    return ccType == null ? null : ccType.getName();
  }
}
