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
import com.ibm.whc.deid.shared.pojo.config.masking.ZIPCodeMaskingProviderConfig;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.ZIPCodeManager;

public class ZIPCodeIdentifier extends AbstractManagerBasedIdentifier {
	/** */
	private static final long serialVersionUID = 4597599832229998583L;

	private ZIPCodeManager zipCodeManager;
	private static final String[] appropriateNames = { "ZIP code", "ZIP", "ZIPCODE" };

	protected volatile boolean initialized = false;

	public ZIPCodeIdentifier(String tenantId, String localizationProperty) {
		super(tenantId, localizationProperty);
	}

	@Override
	public ProviderType getType() {
		return ProviderType.ZIPCODE;
	}

	@Override
	public String getDescription() {
		return "ZIP code identification.";
	}

	@Override
	public ValueClass getValueClass() {
		return ValueClass.TEXT;
	}

	@Override
	protected Manager getManager() {
		if (!initialized) {
			zipCodeManager = (ZIPCodeManager) ManagerFactory.getInstance().getManager(tenantId, Resource.ZIPCODE,
                ZIPCodeMaskingProviderConfig.MASK_PREFIX_LENGTH_DEFAULT, localizationProperty);

			initialized = true;
		}
		return zipCodeManager;
	}

	@Override
	protected Collection<String> getAppropriateNames() {
		return Arrays.asList(appropriateNames);
	}
}
