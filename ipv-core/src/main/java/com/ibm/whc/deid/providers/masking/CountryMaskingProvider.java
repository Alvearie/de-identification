/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.Country;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.CountryMaskingProviderConfig;
import com.ibm.whc.deid.util.CountryManager;
import com.ibm.whc.deid.util.CountryNameSpecification;
import com.ibm.whc.deid.util.ManagerFactory;

/**
 * The type Country masking provider.
 *
 */
public class CountryMaskingProvider extends AbstractMaskingProvider {
  
  private static final long serialVersionUID = -4599680318202749305L;

  protected final boolean getClosest;
  protected final int closestK;
  protected final boolean getPseudorandom;
  
  protected transient volatile CountryManager countryResourceManager = null;

  public CountryMaskingProvider(CountryMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty, configuration);
    this.getClosest = configuration.isMaskClosest();
    this.closestK = configuration.getMaskClosestK();
    this.getPseudorandom = configuration.isMaskPseudorandom();
  }

  protected CountryManager getCountryManager() {
    if (countryResourceManager == null) {
      countryResourceManager = (CountryManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.COUNTRY, null, localizationProperty);
    }
    return countryResourceManager;
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    CountryManager countryManager = getCountryManager();

    if (getPseudorandom) {
      return countryManager.getPseudorandom(identifier);
    }

    if (getClosest) {
      String selected = countryManager.getClosestCountry(identifier, closestK);
      if (selected == null) {
        selected = applyUnexpectedValueHandling(identifier, () -> {
          Country randomCountry = countryManager.getRandomValue(CountryNameSpecification.NAME);
          return randomCountry == null ? null : randomCountry.getName();
        });
      }
      return selected;
    }

    Country country = countryManager.getValue(identifier);
    CountryNameSpecification spec =
        country == null ? CountryNameSpecification.NAME : country.getCountryNameSpecification();
    Country selectedCountry = countryManager.getRandomValue(spec);
    return selectedCountry == null ? null : selectedCountry.getName(spec);
  }
}
