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
import com.ibm.whc.deid.util.CountryManager;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;

/**
 * The type Country identifier.
 *
 */
public class CountryIdentifier extends AbstractManagerBasedIdentifier {

	private static final long serialVersionUID = -2813196255656831409L;

	private static final String[] appropriateNames = { "Country" };

    protected transient volatile CountryManager countryManager = null;

	@Override
	public ProviderType getType() {
		return ProviderType.COUNTRY;
	}

	@Override
	public String getDescription() {
		return "Country identification. Countries can be recognized by either their name (like United States of America)"
				+ ", their 2-letter ISO code (like US) or their 3-letter ISO code (like USA)";
	}

	public CountryIdentifier(String tenantId, String localizationProperty) {
		super(tenantId, localizationProperty);
	}

	@Override
	public Collection<ProviderType> getLinkedTypes() {
		return Arrays.asList(new ProviderType[] { ProviderType.CITY });
	}

	@Override
	public ValueClass getValueClass() {
		return ValueClass.LOCATION;
	}

    @Override
    protected Manager getManager() {
      if (countryManager == null) {
        countryManager = (CountryManager) ManagerFactory.getInstance().getManager(tenantId,
            Resource.COUNTRY, null, localizationProperty);
      }
      return countryManager;
    }

	@Override
	protected Collection<String> getAppropriateNames() {
		return Arrays.asList(appropriateNames);
	}
}
