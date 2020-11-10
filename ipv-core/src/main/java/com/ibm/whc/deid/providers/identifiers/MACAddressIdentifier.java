/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.regex.Pattern;

public class MACAddressIdentifier extends AbstractRegexBasedIdentifier {
  /** */
  private static final long serialVersionUID = -2418921611793083228L;

  private static final Collection<Pattern> macAddressPatterns = new ArrayList<Pattern>(
      Arrays.asList(Pattern.compile("^([0-9a-fA-F][0-9a-fA-F]:){5}([0-9a-fA-F][0-9a-fA-F])$")));

  private static final String[] appropriateNames = {"MAC", "MAC Address"};

  @Override
  protected Collection<Pattern> getPatterns() {
    return macAddressPatterns;
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }

  @Override
  public ProviderType getType() {
    return ProviderType.MAC_ADDRESS;
  }

  @Override
  public String getDescription() {
    return "MAC Address identification";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }
}
