/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.Continent;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.ContinentMaskingProviderConfig;
import com.ibm.whc.deid.util.ContinentManager;
import com.ibm.whc.deid.util.ManagerFactory;

/**
 * The type Continent masking provider.
 *
 */
public class ContinentMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -4463664508295286291L;

  protected final boolean getClosest;
  protected final int getClosestK;

  protected transient volatile ContinentManager continentResourceManager = null;

  public ContinentMaskingProvider(ContinentMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty, configuration);
    this.getClosest = configuration.isMaskClosest();
    this.getClosestK = configuration.getMaskClosestK();
  }

  protected ContinentManager getContinentManager() {
    if (continentResourceManager == null) {
      continentResourceManager = (ContinentManager) ManagerFactory.getInstance()
          .getManager(tenantId, Resource.CONTINENT, null, localizationProperty);
    }
    return continentResourceManager;
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    ContinentManager continentManager = getContinentManager();

    if (getClosest) {
      Continent continent = continentManager.getValue(identifier);
      if (continent == null || continent.getLocation() == null) {
        return applyUnexpectedValueHandling(identifier,
            () -> getRandomContinentName(continentManager));
      }
      Continent selected = continentManager.getClosestContinent(continent, getClosestK);
      return selected == null ? null : selected.getName();
    }

    return getRandomContinentName(continentManager);
  }

  protected String getRandomContinentName(ContinentManager manager) {
    Continent randomContinent = manager.getRandomValue();
    return randomContinent == null ? null : randomContinent.getName();
  }
}
