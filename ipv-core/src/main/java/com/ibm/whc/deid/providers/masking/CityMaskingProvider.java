/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.City;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.CityMaskingProviderConfig;
import com.ibm.whc.deid.util.CityManager;
import com.ibm.whc.deid.util.ManagerFactory;


/**
 * The type City masking provider.
 *
 */
public class CityMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 2388319621060552438L;

  protected final boolean getClosest;
  protected final int closestK;
  protected final boolean getPseudorandom;

  protected transient volatile CityManager cityResourceManager = null;

  public CityMaskingProvider(CityMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty, configuration);
    this.getClosest = configuration.isMaskClosest();
    this.closestK = configuration.getMaskClosestK();
    this.getPseudorandom = configuration.isMaskPseudorandom();
  }

  protected CityManager getCityManager() {
    if (cityResourceManager == null) {
      cityResourceManager = (CityManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.CITY, null, localizationProperty);
    }
    return cityResourceManager;
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    CityManager cityManager = getCityManager();

    if (getPseudorandom) {
      return cityManager.getPseudorandom(identifier);
    }

    if (getClosest) {
      City inputCity = cityManager.getValue(identifier);
      if (inputCity == null || inputCity.getLocation() == null) {
        return applyUnexpectedValueHandling(identifier, () -> getRandomCityName(cityManager));
      }
      City selected = cityManager.getClosestCity(inputCity, closestK);
      return selected == null ? null : selected.getName();
    }

    return getRandomCityName(cityManager);
  }

  protected String getRandomCityName(CityManager manager) {
    City city = manager.getRandomValue();
    return city == null ? null : city.getName();
  }
}
