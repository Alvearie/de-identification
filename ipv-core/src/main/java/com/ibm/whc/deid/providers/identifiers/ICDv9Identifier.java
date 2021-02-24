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
import com.ibm.whc.deid.util.ICDv9Manager;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;

/**
 * The type Ic dv 9 identifier.
 *
 */
public class ICDv9Identifier extends AbstractManagerBasedIdentifier {
	/** */
	private static final long serialVersionUID = -23792857075066172L;

	private static final String[] appropriateNames = { "ICD", "Disease code", "ICD9" };
	protected volatile boolean initialized = false;

	private ICDv9Manager icdv9Manager;

	@Override
	public ProviderType getType() {
		return ProviderType.ICDv9;
	}

	@Override
	public String getDescription() {
		return "ICD9 identification";
	}

	@Override
	public ValueClass getValueClass() {
		return ValueClass.TEXT;
	}

	@Override
	protected Manager getManager() {
		if (!initialized) {
			icdv9Manager = (ICDv9Manager) ManagerFactory.getInstance().getManager(tenantId, Resource.ICDV9, null,
					localizationProperty);

			initialized = true;
		}
		return icdv9Manager;
	}

	@Override
	protected Collection<String> getAppropriateNames() {
		return Arrays.asList(appropriateNames);
	}
}
