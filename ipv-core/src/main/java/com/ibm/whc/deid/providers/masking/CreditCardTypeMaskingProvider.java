/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import java.util.Map;

import com.ibm.whc.deid.models.CreditCard;
import com.ibm.whc.deid.models.OriginalMaskedValuePair;
import com.ibm.whc.deid.schema.FieldRelationship;
import com.ibm.whc.deid.util.CreditCardManager;
import com.ibm.whc.deid.util.CreditCardTypeManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class CreditCardTypeMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = 3375383479009603851L;

	private static final CreditCardTypeManager ccTypeManager = new CreditCardTypeManager(
			LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);
  private static final CreditCardManager creditCardManager = new CreditCardManager();

  /** Instantiates a new Credit card type masking provider. */
  public CreditCardTypeMaskingProvider() {
		this(new SecureRandom());
  }

  /**
   * Instantiates a new Credit card type masking provider.
   *
   * @param random the random
   * @param configuration the configuration
   */
	public CreditCardTypeMaskingProvider(SecureRandom random) {
	}

  @Override
  public String mask(String identifier, String fieldName, FieldRelationship fieldRelationship,
      Map<String, OriginalMaskedValuePair> values) {
    try {
      String ccFieldName = fieldRelationship.getOperands()[0].getName();
      String maskedCC = values.get(ccFieldName).getMasked();
      CreditCard creditCard = creditCardManager.lookupInfo(maskedCC);

      return (creditCard == null) ? mask(identifier) : creditCard.getName();
    } catch (Exception e) {
      logException(e);
      return null;
    }
  }

  @Override
  public String mask(String identifier) {
    return ccTypeManager.getRandomKey();
  }
}
