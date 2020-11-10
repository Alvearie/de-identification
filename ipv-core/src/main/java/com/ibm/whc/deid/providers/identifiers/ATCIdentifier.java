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
import com.ibm.whc.deid.util.ATCManager;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;

public class ATCIdentifier extends AbstractManagerBasedIdentifier {
	/** */
	private static final long serialVersionUID = 4135301254440626321L;

	private ATCManager atcManager;

	private final String[] appropriateNames = new String[] { "ATC" };

	protected volatile boolean initialized = false;

	@Override
	protected Manager getManager() {
		if (!initialized) {
			atcManager = (ATCManager) ManagerFactory.getInstance().getManager(null, Resource.ATC_CODES, null);

			initialized = true;
		}
		return atcManager;
	}

	@Override
	protected Collection<String> getAppropriateNames() {
		return Arrays.asList(appropriateNames);
	}

	@Override
	public ProviderType getType() {
		return ProviderType.ATC;
	}

	@Override
	public String getDescription() {
		return "ATC identification";
	}

	@Override
	public ValueClass getValueClass() {
		return ValueClass.TEXT;
	}
}
