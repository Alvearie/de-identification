/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

public class PluggableLookupIdentifier extends AbstractIdentifier {
  /** */
  private static final long serialVersionUID = -5353746780992306599L;

  private final Collection<String> appropriateNames;
  private final ProviderType providerType;
  private final ValueClass valueClass;
  private final Set<String> valueSet;
  private final boolean ignoreCase;

  /**
   * Instantiates a new Pluggable lookup identifier.
   *
   * @param providerTypeName the provider type name
   * @param appropriateNames the appropriate names
   * @param values the values
   * @param ignoreCase the ignore case
   * @param valueClass the value class
   */
  public PluggableLookupIdentifier(String providerTypeName, Collection<String> appropriateNames,
      Collection<String> values, boolean ignoreCase, ValueClass valueClass) {

    this.appropriateNames = appropriateNames;
    this.providerType = ProviderType.valueOf(providerTypeName);
    this.valueClass = valueClass;
    this.ignoreCase = ignoreCase;

    this.valueSet = new HashSet<>();
    for (String p : values) {
      if (ignoreCase) {
        this.valueSet.add(p.toUpperCase());
      } else {
        this.valueSet.add(p);
      }
    }
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
  public boolean isOfThisType(String data) {
    if (ignoreCase) {
      return this.valueSet.contains(data.toUpperCase());
    } else {
      return this.valueSet.contains(data);
    }
  }

  @Override
  public String getDescription() {
    return "Pluggable lookup-based identifier";
  }

  @Override
  public ValueClass getValueClass() {
    return this.valueClass;
  }
}
