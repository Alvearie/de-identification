/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.util.IdentifierUtils;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.regex.Pattern;

/**
 * The type Credit card identifier.
 *
 */
public class CreditCardIdentifier extends AbstractRegexBasedIdentifier {
  /** */
  private static final long serialVersionUID = 7606629703464582353L;

  private static final String[] appropriateNames = {"CreditCard", "Credit Card", "CCN"};

  private static final Collection<Pattern> combinedPattern =
      new ArrayList<>(Arrays.asList(Pattern.compile("^(?:4[0-9]{12}(?:[0-9]{3})?)" + // visa
          "|(?:5[1-5][0-9]{14})" + // MasterCard
          "|(?:3[4-7][0-9]{13,14})" + // American Express
          "|(?:3(?:0[0-5]|[68][0-9])[0-9]{11})" + // Diners Club
          "|(?:6(?:011|5[0-9]{2})[0-9]{12})" + // Discover
          "|(?:(?:(?:2131)|(?:1800)|(?:35)\\d{3})[0-9]{3,}" + // JCB
          ")$")));

  @Override
  public String getDescription() {
    return "Credit card identification. Cards detected are VISA, Mastercard, AMEX, Diners Club, Discover and JCB";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }

  @Override
  public ProviderType getType() {
    return ProviderType.CREDIT_CARD;
  }

  @Override
  protected Collection<Pattern> getPatterns() {
    return combinedPattern;
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }

  @Override
  public boolean isOfThisType(String identifier) {
    if (!matches(identifier) || !IdentifierUtils.checkLastDigit(identifier)) {
      return false;
    }

    return true;
  }
}
