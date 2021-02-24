/*
 * (C) Copyright IBM Corp. 2016,2020
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
  /** */
  private static final long serialVersionUID = 3989458440094688035L;

	private GenderManager genderManager;

	protected volatile boolean initialized = false;

  @Override
  protected Manager getManager() {
		if (!initialized) {
			genderManager = (GenderManager) ManagerFactory.getInstance().getManager(tenantId, Resource.GENDER, null,
					localizationProperty);

			initialized = true;
		}
		return genderManager;
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
