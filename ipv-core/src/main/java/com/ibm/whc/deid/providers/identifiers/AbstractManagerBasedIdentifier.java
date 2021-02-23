/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.util.Manager;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public abstract class AbstractManagerBasedIdentifier extends AbstractIdentifier {
  /** */
  private static final long serialVersionUID = 2133606795721693469L;

	protected String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

	protected String tenantId;

	public AbstractManagerBasedIdentifier(String tenantId, String localizationProperty) {
		this.localizationProperty = localizationProperty;
		this.tenantId = tenantId;
	}

	public AbstractManagerBasedIdentifier() {

	}

  /**
   * Gets manager.
   *
   * @return the manager
   */
  protected abstract Manager getManager();

  @Override
  public boolean isOfThisType(String identifier) {
    return getManager().isValidKey(identifier);
  }
}
