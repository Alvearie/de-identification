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
import com.ibm.whc.deid.util.HospitalManager;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;

public class HospitalIdentifier extends AbstractManagerBasedIdentifier {

	private static final long serialVersionUID = -4815875246610729706L;

	private final String[] appropriateNames = { "Hospital", "Medical Center" };

    protected transient volatile HospitalManager hospitalResourceManager = null;

	public HospitalIdentifier(String tenantId, String localizationProperty) {
		super(tenantId, localizationProperty);
	}

	@Override
	protected Manager getManager() {
      if (hospitalResourceManager == null) {
        hospitalResourceManager = (HospitalManager) ManagerFactory.getInstance()
            .getManager(tenantId, Resource.HOSPITAL_NAMES, null, localizationProperty);
      }
      return hospitalResourceManager;
	}

	@Override
	protected Collection<String> getAppropriateNames() {
		return Arrays.asList(appropriateNames);
	}

	@Override
	public ProviderType getType() {
		return ProviderType.HOSPITAL;
	}

	@Override
	public String getDescription() {
		return "Hospital and medical center name identification";
	}

	@Override
	public ValueClass getValueClass() {
		return ValueClass.TEXT;
	}
}
