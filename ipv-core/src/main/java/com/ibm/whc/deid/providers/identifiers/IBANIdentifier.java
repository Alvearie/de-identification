/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import java.util.Arrays;
import java.util.Collection;
import org.iban4j.Iban;

public class IBANIdentifier extends AbstractIdentifier {
  /** */
  private static final long serialVersionUID = -33659335503877L;

  private static final String[] appropriateNames = new String[] {"IBAN"};

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }

  @Override
  public ProviderType getType() {
    return ProviderType.IBAN;
  }

  @Override
  public boolean isOfThisType(String data) {
    try {
      Iban.valueOf(data);
      return true;
    } catch (Exception ignored) {
    }

    return false;
  }

  @Override
  public String getDescription() {
    return "IBAN identifier";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }
}
