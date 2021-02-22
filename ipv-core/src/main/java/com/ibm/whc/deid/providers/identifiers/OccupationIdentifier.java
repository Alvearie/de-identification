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
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.OccupationManager;

public class OccupationIdentifier extends AbstractManagerBasedIdentifier {
	/** */
	private static final long serialVersionUID = 8852489168068969429L;

	private static final String[] appropriateNames = { "Job", "Occupation" };
	private OccupationManager occupationManager;
	protected volatile boolean initialized = false;

	@Override
	protected Manager getManager() {
		if (!initialized) {
			occupationManager = (OccupationManager) ManagerFactory.getInstance().getManager(null, Resource.OCCUPATION,
					null, localizationProperty);

			initialized = true;
		}
		return occupationManager;
	}

	@Override
	protected Collection<String> getAppropriateNames() {
		return Arrays.asList(appropriateNames);
	}

	@Override
	public ProviderType getType() {
		return ProviderType.OCCUPATION;
	}

	@Override
	public String getDescription() {
		return "Occupation identification";
	}

	@Override
	public ValueClass getValueClass() {
		return ValueClass.TEXT;
	}
}
