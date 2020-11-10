/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.providers.ProviderType;
import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;

public abstract class AbstractIdentifier implements Identifier, Serializable {
  /** */
  private static final long serialVersionUID = 439172843528253186L;
  private String maskingRule;

  public String getMaskingRule() {
    return maskingRule;
  }

  public void setMaskingRule(String maskingRule) {
    this.maskingRule = maskingRule;
  }

  /**
   * Gets appropriate names.
   *
   * @return the appropriate names
   */
  protected Collection<String> getAppropriateNames() {
    return Collections.emptyList();
  };

  @Override
  public int getPriority() {
    return 100;
  }

  @Override
  public Collection<ProviderType> getLinkedTypes() {
    return null;
  }

  @Override
  public boolean isAppropriateName(String fieldName) {
    Collection<String> appropriateNames = getAppropriateNames();
    String fieldNameLowercase = fieldName.toLowerCase();

    for (String name : appropriateNames) {
      if (name.toLowerCase().equals(fieldNameLowercase)) {
        return true;
      }
    }
    return false;
  }

  protected boolean isTitlecase(String token) {
    if (token.isEmpty()) {
      return true;
    }

    if (token.startsWith("(") && token.endsWith(")")) {
      token = token.substring(1, token.length() - 1);
    }

    if (token.isEmpty()) {
      return true;
    }

    if (!Character.isUpperCase(token.charAt(0))) {
      return false;
    }

    int lowercaseCount = 0;

    for (int i = 1; i < token.length(); i++) {
      if (Character.isLetter(token.charAt(i)) && Character.isLowerCase(token.charAt(i))) {
        lowercaseCount++;
      }
    }

    return lowercaseCount > 0;
  }

  protected boolean isAllUppercase(String token) {
    if (token.isEmpty()) {
      return true;
    }

    for (int i = 0; i < token.length(); i++) {
      if (Character.isLetter(token.charAt(i)) && !Character.isUpperCase(token.charAt(i))) {
        return false;
      }
    }

    return true;
  }
}
