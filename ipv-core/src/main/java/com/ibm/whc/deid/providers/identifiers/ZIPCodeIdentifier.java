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
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.ZIPCodeManager;

public class ZIPCodeIdentifier extends AbstractManagerBasedIdentifier {

  private static final long serialVersionUID = 4597599832229998583L;

  private static final String[] appropriateNames = {"ZIP code", "ZIP", "ZIPCODE"};

  protected transient volatile ZIPCodeManager zipCodeResourceManager = null;

  public ZIPCodeIdentifier(String tenantId, String localizationProperty) {
    super(tenantId, localizationProperty);
  }

  @Override
  public ProviderType getType() {
    return ProviderType.ZIPCODE;
  }

  @Override
  public String getDescription() {
    return "ZIP code identification.";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }

  @Override
  protected Manager getManager() {
    if (zipCodeResourceManager == null) {
      zipCodeResourceManager = (ZIPCodeManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.ZIPCODE, null, localizationProperty);
    }
    return zipCodeResourceManager;
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }
}
