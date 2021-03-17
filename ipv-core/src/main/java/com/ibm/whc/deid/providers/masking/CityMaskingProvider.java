/*
 * (C) Copyright IBM Corp. 2016,2021
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
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  public CityMaskingProvider(CityMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty);
    this.getClosest = configuration.isMaskClosest();
    this.closestK = configuration.getMaskClosestK();
    this.getPseudorandom = configuration.isMaskPseudorandom();
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  protected CityManager getCityManager() {
    return (CityManager) ManagerFactory.getInstance().getManager(tenantId, Resource.CITY, null,
        localizationProperty);
  }

  @Override
  public String mask(String identifier) {
    CityManager cityManager = getCityManager();

    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    if (getPseudorandom) {
      return cityManager.getPseudorandom(identifier);
    }

    if (getClosest) {
      return cityManager.getClosestCity(identifier, this.closestK);
    }

    City city = cityManager.getValue(identifier);

    if (city == null) {
      debugFaultyInput("city");
      if (unspecifiedValueHandling == 2) {
        return cityManager.getRandomKey();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    return cityManager.getRandomKey(city.getNameCountryCode());
  }
}
