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
import com.ibm.whc.deid.util.ICDv9Manager;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;

/**
 * Class that identifies ICDv9 codes.
 */
public class ICDv9Identifier extends AbstractManagerBasedIdentifier {

	private static final long serialVersionUID = -23792857075066172L;

	private static final String[] appropriateNames = { "ICD", "Disease code", "ICD9" };

    protected transient volatile ICDv9Manager icdv9Manager = null;

	public ICDv9Identifier(String tenantId, String localizationProperty) {
		super(tenantId, localizationProperty);
	}

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
      if (icdv9Manager == null) {
        icdv9Manager =
            (ICDv9Manager) ManagerFactory.getInstance().getManager(tenantId, Resource.ICDV9, null,
					localizationProperty);
      }
      return icdv9Manager;
	}

	@Override
	protected Collection<String> getAppropriateNames() {
		return Arrays.asList(appropriateNames);
	}
}
