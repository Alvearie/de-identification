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
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.MaritalStatusManager;

public class MaritalStatusIdentifier extends AbstractManagerBasedIdentifier {

	private static final long serialVersionUID = 8801786121068734933L;

	private Collection<String> appropriateNames = Arrays.asList(new String[] { "Marital Status" });

    protected transient volatile MaritalStatusManager maritalStatusResourceManager = null;
	
	public MaritalStatusIdentifier(String tenantId, String localizationProperty) {
		super(tenantId, localizationProperty);
	}

	@Override
    protected MaritalStatusManager getManager() {
      if (maritalStatusResourceManager == null) {
        maritalStatusResourceManager = (MaritalStatusManager) ManagerFactory.getInstance()
            .getManager(tenantId, Resource.MARITAL_STATUS, null, localizationProperty);
      }
      return maritalStatusResourceManager;
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
