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

public class SSNUKIdentifier extends AbstractIdentifier {
  /** */
  private static final long serialVersionUID = 2822166053669322303L;

  /** The Appropriate names. */
  static final String[] appropriateNames = {"SSN"};

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }

  @Override
  public ProviderType getType() {
    return ProviderType.SSN_UK;
  }

  @Override
  public boolean isOfThisType(String data) {
    String ssn = data.replace(" ", "").toUpperCase();
    if (ssn.length() != 9) {
      return false;
    }

    char first = ssn.charAt(0);
    char second = ssn.charAt(1);
    if (!Character.isAlphabetic(first) || !Character.isAlphabetic(second)) {
      return false;
    }

    if (first == 'D' || first == 'F' || first == 'I' || first == 'Q' || first == 'U'
        || first == 'V') {
      return false;
    }

    if (second == 'D' || second == 'F' || second == 'I' || second == 'Q' || second == 'U'
        || second == 'V' || second == 'O') {
      return false;
    }

    for (int i = 2; i < 8; i++) {
      if (!Character.isDigit(ssn.charAt(i))) {
        return false;
      }
    }

    char last = ssn.charAt(8);
    if (last < 'A' || last > 'D') {
      return false;
    }

    return true;
  }

  @Override
  public String getDescription() {
    return "SSN identification for UK";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }
}
