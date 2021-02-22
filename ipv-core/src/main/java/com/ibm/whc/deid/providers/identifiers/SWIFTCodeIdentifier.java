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
import com.ibm.whc.deid.util.SWIFTCodeManager;

public class SWIFTCodeIdentifier extends AbstractManagerBasedIdentifier {
  /** */
  private static final long serialVersionUID = -2241208945295394974L;

  private static final String[] appropriateNames = {"SWIFT"};

	protected volatile boolean initialized = false;
	private SWIFTCodeManager swiftCodeManager;

  @Override
  protected Manager getManager() {
		if (!initialized) {
			swiftCodeManager = (SWIFTCodeManager) ManagerFactory.getInstance().getManager(null, Resource.SWIFT, null, localizationProperty);

			initialized = true;
		}
    return swiftCodeManager;
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }

  @Override
  public ProviderType getType() {
    return ProviderType.SWIFT;
  }

  @Override
  public String getDescription() {
    return "SWIFT Code identification";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }
}
