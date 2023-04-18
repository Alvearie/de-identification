/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import java.util.Arrays;
import java.util.Collection;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.IMEIManager;
import com.ibm.whc.deid.util.IdentifierUtils;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;

public class IMEIIdentifier extends AbstractManagerBasedIdentifier {

  private static final long serialVersionUID = -8398288614698641845L;

  private static final String[] appropriateNames = new String[] {"IMEI"};

  protected transient volatile IMEIManager imeiManager = null;

  public IMEIIdentifier(String tenantId, String localizationProperty) {
    super(tenantId, localizationProperty);
  }

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
		if (!getManager().isValidKey(tac)) {
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

  @Override
  protected Manager getManager() {
    if (imeiManager == null) {
      imeiManager = (IMEIManager) ManagerFactory.getInstance().getManager(tenantId, Resource.TACDB,
          null, localizationProperty);
    }
    return imeiManager;
  }
}
