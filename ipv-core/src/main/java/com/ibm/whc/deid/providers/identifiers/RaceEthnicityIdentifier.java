/*
 * (C) Copyright IBM Corp. 2016,2021
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
 * The type race-ethnicity identifier.
 *
 */
public class RaceEthnicityIdentifier extends AbstractManagerBasedIdentifier {

	private static final long serialVersionUID = -2694410982440148058L;

	private static final String[] appropriateNames = { "Race", "Ethnicity" };

    protected transient volatile RaceManager raceManager = null;

	public RaceEthnicityIdentifier(String tenantId, String localizationProperty) {
		super(tenantId, localizationProperty);
	}

	@Override
	public ProviderType getType() {
		return ProviderType.RACE;
	}

	@Override
	protected Manager getManager() {
      if (raceManager == null) {
        raceManager = (RaceManager) ManagerFactory.getInstance().getManager(tenantId,
            Resource.RACE_ETHNICITY, null, localizationProperty);
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
