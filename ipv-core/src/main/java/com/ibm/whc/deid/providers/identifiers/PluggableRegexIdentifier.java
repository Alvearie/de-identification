/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import java.util.ArrayList;
import java.util.Collection;
import java.util.regex.Pattern;

public class PluggableRegexIdentifier extends AbstractRegexBasedIdentifier {
  /** */
  private static final long serialVersionUID = 3589841119499969992L;

  private final Collection<String> appropriateNames;
  private Collection<Pattern> patterns;
  private final ProviderType providerType;
  private final ValueClass valueClass;

  /**
   * Instantiates a new Pluggable regex identifier.
   *
   * @param providerTypeName the provider type name
   * @param appropriateNames the appropriate names
   * @param patternStrings the pattern strings
   * @param valueClass the value class
   */
  public PluggableRegexIdentifier(String providerTypeName, Collection<String> appropriateNames,
      Collection<String> patternStrings, ValueClass valueClass) {

    this.appropriateNames = appropriateNames;
    this.providerType = ProviderType.valueOf(providerTypeName);
    this.valueClass = valueClass;

    this.patterns = new ArrayList<>(patternStrings.size());
    for (String p : patternStrings) {
      this.patterns.add(Pattern.compile(p));
    }
  }

  @Override
  protected Collection<Pattern> getPatterns() {
    return this.patterns;
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return appropriateNames;
  }

  @Override
  public ProviderType getType() {
    return this.providerType;
  }

  @Override
  public String getDescription() {
    return "Pluggable identifier";
  }

  @Override
  public ValueClass getValueClass() {
    return this.valueClass;
  }
}
