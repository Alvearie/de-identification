/*
 * (C) Copyright IBM Corp. 2016,2020
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
  /** */
  private static final long serialVersionUID = 2388319621060552438L;

  protected CityManager cityManager;
  protected final boolean getClosest;
  protected final int closestK;
  protected final boolean getPseudorandom;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  protected volatile boolean initialized = false;

  public CityMaskingProvider(CityMaskingProviderConfig configuration, String tenantId, String localizationProperty) {
		super(tenantId, localizationProperty);
    this.getClosest = configuration.isMaskClosest();
    this.closestK = configuration.getMaskClosestK();
    this.getPseudorandom = configuration.isMaskPseudorandom();
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  protected void initialize() {
    if (!initialized) {
      cityManager =
					(CityManager) ManagerFactory.getInstance().getManager(tenantId, Resource.CITY, null,
							localizationProperty);

      initialized = true;
    }
  }

  @Override
  public String mask(String identifier) {
    initialize();
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

    City city = cityManager.getKey(identifier);

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
