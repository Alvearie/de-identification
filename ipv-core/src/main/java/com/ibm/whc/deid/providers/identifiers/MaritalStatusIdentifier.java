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
import com.ibm.whc.deid.util.MaritalStatusManager;

public class MaritalStatusIdentifier extends AbstractManagerBasedIdentifier {
	/** */
	private static final long serialVersionUID = 8801786121068734933L;

	private Collection<String> appropriateNames = Arrays.asList(new String[] { "Marital Status" });

	protected volatile boolean initialized = false;
	private MaritalStatusManager maritalStatusManager;

	@Override
	protected Manager getManager() {
		if (!initialized) {
			maritalStatusManager = (MaritalStatusManager) ManagerFactory.getInstance().getManager(null,
					Resource.MARITAL_STATUS, null);

			initialized = true;
		}
		return maritalStatusManager;
	}

	@Override
	protected Collection<String> getAppropriateNames() {
		return this.appropriateNames;
	}

	@Override
	public ProviderType getType() {
		return ProviderType.MARITAL_STATUS;
	}

	@Override
	public String getDescription() {
		return "Marital status identifier";
	}

	@Override
	public ValueClass getValueClass() {
		return ValueClass.TEXT;
	}
}
