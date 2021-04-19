/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.GenderManager;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;

public class GenderIdentifier extends AbstractManagerBasedIdentifier {

  private static final long serialVersionUID = 3989458440094688035L;

  protected transient volatile GenderManager genderResourceManager = null;

  public GenderIdentifier(String tenantId, String localizationProperty) {
    super(tenantId, localizationProperty);
  }

  @Override
  protected Manager getManager() {
    if (genderResourceManager == null) {
      genderResourceManager = (GenderManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.GENDER, null, localizationProperty);
    }
    return genderResourceManager;
  }

  @Override
  public ProviderType getType() {
    return ProviderType.GENDER;
  }

  @Override
  public String getDescription() {
    return "Gender identification";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }
}
