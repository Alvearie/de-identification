/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.CountyManager;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;

public class CountyIdentifier extends AbstractManagerBasedIdentifier {
	/** */
	private static final long serialVersionUID = -8277356320214479220L;

	private CountyManager countyManager;

	protected volatile boolean initialized = false;

	@Override
	protected Manager getManager() {

		if (!initialized) {
			countyManager = (CountyManager) ManagerFactory.getInstance().getManager(tenantId, Resource.COUNTY, null,
					localizationProperty);

			initialized = true;
		}
		return countyManager;
	}

	@Override
	public ProviderType getType() {
		return ProviderType.COUNTY;
	}

	@Override
	public String getDescription() {
		return "County Identification";
	}

	@Override
	public ValueClass getValueClass() {
		return ValueClass.TEXT;
	}
}
