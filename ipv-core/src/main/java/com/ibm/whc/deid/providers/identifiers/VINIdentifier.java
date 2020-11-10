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
import com.ibm.whc.deid.util.VINManager;

/*
 * VIN: classification There are at least four competing standards used to calculate VIN.
 *
 * FMVSS 115, Part 565: Used in United States and Canada[2] ISO Standard 3779: Used in Europe and
 * many other parts of the world SAE J853: Very similar to the ISO standard ADR 61/2 used in
 * Australia, referring back to ISO 3779 and 3780.[3]
 *
 * https://en.wikipedia.org/wiki/Vehicle_identification_number
 */

public class VINIdentifier extends AbstractManagerBasedIdentifier {
	/** */
	private static final long serialVersionUID = -1445672279609421242L;

	protected volatile boolean initialized = false;

	private VINManager vinManager;

	private static final String[] appropriateNames = { "Vehicle Identification Number", "VIN" };

	@Override
	public ProviderType getType() {
		return ProviderType.VIN;
	}

	@Override
	public String getDescription() {
		return "Vehicle identification number identification. Supports world manufacturer identification";
	}

	@Override
	public ValueClass getValueClass() {
		return ValueClass.TEXT;
	}

	@Override
	protected Manager getManager() {
		if (!initialized) {
			vinManager = (VINManager) ManagerFactory.getInstance().getManager(null,
					Resource.WORLD_MANUFACTURERS_IDENTIFIER, null);

			initialized = true;
		}
		return vinManager;
	}

	@Override
	protected Collection<String> getAppropriateNames() {
		return Arrays.asList(appropriateNames);
	}
}
