/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;

public class NumericIdentifier extends AbstractIdentifier {
  /** */
  private static final long serialVersionUID = -8842392935894562325L;

  @Override
  public int getPriority() {
    return 0;
  }

  @Override
  public ProviderType getType() {
    return ProviderType.NUMERIC;
  }

  @Override
  public boolean isOfThisType(String data) {
    try {
      Double.parseDouble(data);
      return true;
    } catch (Exception ignored) {
    }

    return false;
  }

  @Override
  public String getDescription() {
    return "Numeric description";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.NUMERIC;
  }
}
