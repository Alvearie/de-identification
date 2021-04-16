/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.StatesUSManager;

public class StatesUSIdentifier extends AbstractManagerBasedIdentifier {

	private static final long serialVersionUID = 710170960599545348L;

    protected transient volatile StatesUSManager statesUSManager = null;

	public StatesUSIdentifier(String tenantId, String localizationProperty) {
		super(tenantId, localizationProperty);
	}

	@Override
	protected Manager getManager() {
      if (statesUSManager == null) {
        statesUSManager = (StatesUSManager) ManagerFactory.getInstance().getManager(tenantId,
            Resource.STATES_US, null, localizationProperty);
      }
      return statesUSManager;
	}

	@Override
	public ProviderType getType() {
		return ProviderType.STATES_US;
	}

	@Override
	public String getDescription() {
		return "Identifies US states";
	}

	@Override
	public ValueClass getValueClass() {
		return ValueClass.TEXT;
	}
}
