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
import com.ibm.whc.deid.util.ContinentManager;
import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.ManagerFactory;

/**
 * The type Continent identifier.
 *
 */
public class ContinentIdentifier extends AbstractManagerBasedIdentifier {
  /** */
  private static final long serialVersionUID = -7174497158361912040L;

  private static final String[] appropriateNames = {"Continent"};
	protected volatile boolean initialized = false;
	private ContinentManager continentManager;

  @Override
  public ProviderType getType() {
    return ProviderType.CONTINENT;
  }

  @Override
  public String getDescription() {
    return "Continent identification";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.LOCATION;
  }

  @Override
  protected Manager getManager() {
		if (!initialized) {
			continentManager = (ContinentManager) ManagerFactory.getInstance().getManager(null, Resource.CONTINENT,
					null);

			initialized = true;
		}
    return continentManager;
  }

  @Override
  public Collection<ProviderType> getLinkedTypes() {
    return Arrays.asList(new ProviderType[] {ProviderType.COUNTRY, ProviderType.CITY});
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }
}
