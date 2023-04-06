/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import java.util.Collection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public abstract class AbstractRegexBasedIdentifier extends AbstractIdentifier {
  /** */
  private static final long serialVersionUID = 7380773192702764303L;

  /**
   * Gets patterns.
   *
   * @return the patterns
   */
  protected abstract Collection<Pattern> getPatterns();

  /**
   * Find match matcher.
   *
   * @param data the data
   * @return the matcher
   */
  public Matcher findMatch(String data) {
    for (Pattern p : getPatterns()) {
      Matcher m = p.matcher(data);
      if (m.matches()) {
        return m;
      }
    }

    return null;
  }

  /**
   * Matches boolean.
   *
   * @param data the data
   * @return the boolean
   */
  public boolean matches(String data) {
    for (Pattern p : getPatterns()) {
      if (p.matcher(data).matches()) {
        return true;
      }
    }

    return false;
  }

  @Override
  public boolean isOfThisType(String identifier) {
    return this.matches(identifier);
  }
}
