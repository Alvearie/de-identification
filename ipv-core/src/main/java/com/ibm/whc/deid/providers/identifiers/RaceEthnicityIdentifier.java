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
import com.ibm.whc.deid.util.RaceManager;

/**
 * The type Race ethnicity identifier.
 *
 */
public class RaceEthnicityIdentifier extends AbstractManagerBasedIdentifier {
	/** */
	private static final long serialVersionUID = -2694410982440148058L;

	private RaceManager raceManager;
	private static final String[] appropriateNames = { "Race", "Ethnicity" };

	protected volatile boolean initialized = false;

	@Override
	public ProviderType getType() {
		return ProviderType.RACE;
	}

	@Override
	protected Manager getManager() {
		if (!initialized) {
			raceManager = (RaceManager) ManagerFactory.getInstance().getManager(null, Resource.RACE_ETHNICITY, null);

			initialized = true;
		}
		return raceManager;
	}

	@Override
	public String getDescription() {
		return "Race/Ethnicity identification of most populous ethnic groups";
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
