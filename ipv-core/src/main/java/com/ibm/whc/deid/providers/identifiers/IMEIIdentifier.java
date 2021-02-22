/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import java.util.Arrays;
import java.util.Collection;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.util.IMEIManager;
import com.ibm.whc.deid.util.IdentifierUtils;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class IMEIIdentifier extends AbstractIdentifier {
  /** */
  private static final long serialVersionUID = -8398288614698641845L;

  private static final String[] appropriateNames = new String[] {"IMEI"};
	private static final IMEIManager imeiManager = new IMEIManager(LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }

  @Override
  public ProviderType getType() {
    return ProviderType.IMEI;
  }

  @Override
  public boolean isOfThisType(String data) {
    int dataLength = data.length();

    if (dataLength < 15 || dataLength > 16) {
      return false;
    }

    for (int i = 0; i < dataLength; i++) {
      if (!Character.isDigit(data.charAt(i))) {
        return false;
      }
    }

    String tac = data.substring(0, 8);
    if (!imeiManager.isValidKey(tac)) {
      return false;
    }

    return IdentifierUtils.checkLastDigit(data);
  }

  @Override
  public String getDescription() {
    return "IMEI identification";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }
}
