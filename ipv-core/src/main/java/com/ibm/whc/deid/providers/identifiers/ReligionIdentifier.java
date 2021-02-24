/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import java.util.Collection;
import java.util.Collections;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.ReligionManager;

public class ReligionIdentifier extends AbstractManagerBasedIdentifier {
	/** */
	private static final long serialVersionUID = -3355052206753857563L;

	private final Collection<String> appropriateNames = Collections.singletonList("Religion");
	private ReligionManager religionManager;
	protected volatile boolean initialized = false;

	@Override
	protected Manager getManager() {
		if (!initialized) {
			religionManager = (ReligionManager) ManagerFactory.getInstance().getManager(tenantId, Resource.RELIGION,
					null, localizationProperty);

			initialized = true;
		}
		return religionManager;
	}

	@Override
	protected Collection<String> getAppropriateNames() {
		return appropriateNames;
	}

	@Override
	public ProviderType getType() {
		return ProviderType.RELIGION;
	}

	@Override
	public String getDescription() {
		return "Religion identifier of the most popular religions";
	}

	@Override
	public ValueClass getValueClass() {
		return ValueClass.TEXT;
	}
}
