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
import com.ibm.whc.deid.util.CityManager;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;

/**
 * The type City identifier.
 *
 */
public class CityIdentifier extends AbstractManagerBasedIdentifier {
	/** */
	private static final long serialVersionUID = -153485244376355769L;

	private static final String[] appropriateNames = { "City" };
	private CityManager cityManager;

	public CityIdentifier(String tenantId, String localizationProperty) {
		super(tenantId, localizationProperty);
	}

	protected volatile boolean initialized = false;

	@Override
	public ProviderType getType() {
		return ProviderType.CITY;
	}

	@Override
	public String getDescription() {
		return "City identification, supports all major cities in the world with population above 100K";
	}

	@Override
	public ValueClass getValueClass() {
		return ValueClass.LOCATION;
	}

	@Override
	protected Manager getManager() {
		if (!initialized) {
			cityManager = (CityManager) ManagerFactory.getInstance().getManager(tenantId, Resource.CITY, null,
					localizationProperty);

			initialized = true;
		}
		return cityManager;
	}

	@Override
	protected Collection<String> getAppropriateNames() {
		return Arrays.asList(appropriateNames);
	}
}
