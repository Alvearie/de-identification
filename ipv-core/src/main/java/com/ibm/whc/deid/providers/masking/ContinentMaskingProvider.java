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
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  protected transient volatile ContinentManager continentResourceManager = null;

  public ContinentMaskingProvider(ContinentMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty);
    this.getClosest = configuration.isMaskClosest();
    this.getClosestK = configuration.getMaskClosestK();
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
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
    try {
      if (identifier == null) {
        debugFaultyInput("identifier");
        return null;
      }

      ContinentManager continentManager = getContinentManager();

      Continent continent = continentManager.getValue(identifier);
      if (continent == null) {
        debugFaultyInput("continent");
        if (unspecifiedValueHandling == 2) {
          Continent randomContinent = continentManager.getRandomValue();
          return randomContinent == null ? null : randomContinent.getName();
        } else if (unspecifiedValueHandling == 3) {
          return unspecifiedValueReturnMessage;
        } else {
          return null;
        }
      }

      if (getClosest) {
        Continent selected = continentManager.getClosestContinent(continent, getClosestK);
        return selected == null ? null : selected.getName();
      }

      Continent randomContinent = continentManager.getRandomValue(continent.getNameCountryCode());
      return randomContinent == null ? null : randomContinent.getName();

    } catch (Exception e) {
      logException(e);
      return null;
    }
  }
}
