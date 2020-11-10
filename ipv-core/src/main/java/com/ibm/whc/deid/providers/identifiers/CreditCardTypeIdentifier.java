/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.util.CreditCardTypeManager;
import com.ibm.whc.deid.util.Manager;
import java.util.Arrays;
import java.util.Collection;

public class CreditCardTypeIdentifier extends AbstractManagerBasedIdentifier {
  /** */
  private static final long serialVersionUID = 8292073040447713030L;

  private static final String[] appropriateNames = {"Credit Card Type"};
  private static final CreditCardTypeManager creditCardTypeManager = new CreditCardTypeManager();

  @Override
  protected Manager getManager() {
    return creditCardTypeManager;
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }

  @Override
  public ProviderType getType() {
    return ProviderType.CREDIT_CARD_TYPE;
  }

  @Override
  public String getDescription() {
    return "Credit Card Type identification";
  }

  @Override
  public Collection<ProviderType> getLinkedTypes() {
    return Arrays.asList(new ProviderType[] {ProviderType.CREDIT_CARD});
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }
}
