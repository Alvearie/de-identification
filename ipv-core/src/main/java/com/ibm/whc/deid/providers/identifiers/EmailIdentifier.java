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

/**
 * The type Email identifier.
 *
 */
public class EmailIdentifier extends AbstractRegexBasedIdentifier {
  /** */
  private static final long serialVersionUID = -7810779537363166322L;

  private static final Collection<Pattern> emailPatterns = new ArrayList<Pattern>(
      Arrays.asList(Pattern.compile("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,4}$")));

  private static final String[] appropriateNames = {"E-mail", "Email"};

  @Override
  public ProviderType getType() {
    return ProviderType.EMAIL;
  }

  @Override
  public String getDescription() {
    return "Email identification. The format is username@domain";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }

  @Override
  protected Collection<Pattern> getPatterns() {
    return emailPatterns;
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }
}
