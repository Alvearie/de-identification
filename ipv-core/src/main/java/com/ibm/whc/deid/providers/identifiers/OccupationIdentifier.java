/*
 * © Merative US L.P. 2016,2021
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
import com.ibm.whc.deid.util.OccupationManager;

/**
 * Class to identify the names of occupations based on names loaded into the service.
 */
public class OccupationIdentifier extends AbstractManagerBasedIdentifier {

	private static final long serialVersionUID = 8852489168068969429L;

	private static final String[] appropriateNames = { "Job", "Occupation" };

    protected transient volatile OccupationManager occupationManager = null;

	public OccupationIdentifier(String tenantId, String localizationProperty) {
		super(tenantId, localizationProperty);
	}

	@Override
	protected Manager getManager() {
      if (occupationManager == null) {
        occupationManager = (OccupationManager) ManagerFactory.getInstance().getManager(tenantId,
            Resource.OCCUPATION, null, localizationProperty);
      }
      return occupationManager;
	}

	@Override
	protected Collection<String> getAppropriateNames() {
		return Arrays.asList(appropriateNames);
	}

	@Override
	public ProviderType getType() {
		return ProviderType.OCCUPATION;
	}

	@Override
	public String getDescription() {
		return "Occupation identification";
	}

	@Override
	public ValueClass getValueClass() {
		return ValueClass.TEXT;
	}
}
