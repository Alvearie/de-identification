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
import com.ibm.whc.deid.util.ICDv10Manager;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;

/**
 * The type Ic dv 10 identifier.
 *
 */
public class ICDv10Identifier extends AbstractManagerBasedIdentifier {
	/** */
	private static final long serialVersionUID = -7484093125257873714L;

	private static final String[] appropriateNames = { "ICD", "Disease code", "ICD10", "ICDv10" };

	protected volatile boolean initialized = false;
	private ICDv10Manager icdv10Manager;

	@Override
	public ProviderType getType() {
		return ProviderType.ICDv10;
	}

	@Override
	public String getDescription() {
		return "ICD-10 identification";
	}

	@Override
	public ValueClass getValueClass() {
		return ValueClass.TEXT;
	}

	@Override
	protected Manager getManager() {
		if (!initialized) {
			icdv10Manager = (ICDv10Manager) ManagerFactory.getInstance().getManager(null, Resource.ICDV10, null);

			initialized = true;
		}

		return icdv10Manager;
	}

	@Override
	protected Collection<String> getAppropriateNames() {
		return Arrays.asList(appropriateNames);
	}
}
