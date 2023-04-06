/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.Collection;

public class URLIdentifier extends AbstractIdentifier {
  /** */
  private static final long serialVersionUID = -6628214973693770648L;

  private static final String[] appropriateNames = {"URL", "Webpage", "Web URL"};

  @Override
  public ProviderType getType() {
    return ProviderType.URL;
  }

  @Override
  public boolean isOfThisType(String data) {
    try {
      new URL(data);
      return true;
    } catch (MalformedURLException ignored) {
    }

    return false;
  }

  @Override
  public String getDescription() {
    return "URL identification. Supports HTTP and HTTPS detection";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }
}
