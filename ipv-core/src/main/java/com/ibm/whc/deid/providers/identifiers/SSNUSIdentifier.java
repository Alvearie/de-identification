/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.models.SSNUS;
import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import java.util.Arrays;
import java.util.Collection;

public class SSNUSIdentifier extends AbstractIdentifier {
  /** */
  private static final long serialVersionUID = 6788331755058512814L;

  private static final String[] appropriateNames = new String[] {"SSN"};

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }

  @Override
  public ProviderType getType() {
    return ProviderType.SSN_US;
  }

  /**
   * Parse ssnus ssnus.
   *
   * @param data the data
   * @return the ssnus
   */
  public SSNUS parseSSNUS(String data) {
    String[] toks = data.split("-");
    String[] ssnParts;

    if (toks.length == 3) {
      if (toks[0].length() != 3 || toks[1].length() != 2 || toks[2].length() != 4) {
        return null;
      }

      ssnParts = toks;
    } else if (toks.length == 1) {
      String candidate = toks[0];
      if (candidate.length() != 9) {
        return null;
      }

      ssnParts = new String[3];
      ssnParts[0] = candidate.substring(0, 3);
      ssnParts[1] = candidate.substring(3, 5);
      ssnParts[2] = candidate.substring(5, 9);
    } else {
      return null;
    }

    for (int i = 0; i < 3; i++) {
      for (int j = 0; j < ssnParts[i].length(); j++) {
        if (!Character.isDigit(ssnParts[i].charAt(j))) {
          return null;
        }
      }
    }

    if (ssnParts[0].equals("000") || ssnParts[0].equals("666")) {
      return null;
    }

    return new SSNUS(ssnParts[0], ssnParts[1], ssnParts[2]);
  }

  @Override
  public boolean isOfThisType(String data) {
    return parseSSNUS(data) != null;
  }

  @Override
  public String getDescription() {
    return "SSN identification for US";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }
}
