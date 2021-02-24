/*
 * (C) Copyright IBM Corp. 2016,2020
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
	/** */
	private static final long serialVersionUID = 710170960599545348L;

	private StatesUSManager statesUSManager = new StatesUSManager(null, localizationProperty);

	protected volatile boolean initialized = false;

	@Override
	protected Manager getManager() {
		if (!initialized) {
			statesUSManager = (StatesUSManager) ManagerFactory.getInstance().getManager(tenantId, Resource.STATES_US,
					null, localizationProperty);

			initialized = true;
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
