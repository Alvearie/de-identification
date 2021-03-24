/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.util.Map;

import com.ibm.whc.deid.models.City;
import com.ibm.whc.deid.models.Country;
import com.ibm.whc.deid.models.OriginalMaskedValuePair;
import com.ibm.whc.deid.schema.FieldRelationship;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.CountryMaskingProviderConfig;
import com.ibm.whc.deid.util.CityManager;
import com.ibm.whc.deid.util.CountryManager;
import com.ibm.whc.deid.util.ManagerFactory;

/**
 * The type Country masking provider.
 *
 */
public class CountryMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -4599680318202749305L;

  protected CountryManager countryManager;
  protected final boolean getClosest;
  protected final int closestK;
  protected final boolean getPseudorandom;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;
  protected CityManager cityManager;

  protected volatile boolean initialized = false;

  public CountryMaskingProvider(CountryMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty);
    this.getClosest = configuration.isMaskClosest();
    this.closestK = configuration.getMaskClosestK();
    this.getPseudorandom = configuration.isMaskPseudorandom();
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  protected void initialize() {
    if (!initialized) {
      countryManager = (CountryManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.COUNTRY, null, localizationProperty);

      cityManager = (CityManager) ManagerFactory.getInstance().getManager(tenantId, Resource.CITY,
          null, localizationProperty);

      initialized = true;
    }
  }

  @Override
  public String mask(String identifier, String fieldName, FieldRelationship fieldRelationship,
      Map<String, OriginalMaskedValuePair> values) {
    initialize();
    try {
      if (identifier == null) {
        debugFaultyInput("identifier");
        return null;
      }

      String cityFieldName = fieldRelationship.getOperands()[0].getName();
      String maskedCity = values.get(cityFieldName).getMasked();

      City city = cityManager.getValue(maskedCity);
      if (city == null) {
        return mask(identifier);
      }

      Country country =
          countryManager.lookupCountry(city.getCountryCode(), city.getNameCountryCode());
      if (country == null) {
        return mask(identifier);
      }

      return country.getName();
    } catch (Exception e) {
      logException(e);
      return null;
    }
  }

  @Override
  public String mask(String identifier) {
    initialize();
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    if (this.getPseudorandom) {
      return countryManager.getPseudorandom(identifier);
    }

    if (getClosest) {
      return countryManager.getClosestCountry(identifier, this.closestK);
    }

    Country country = countryManager.lookupCountry(identifier);

    if (country == null) {
      debugFaultyInput("country");
      if (unspecifiedValueHandling == 2) {
        return countryManager.getRandomKey();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    return countryManager.getRandomKey(identifier, country.getNameCountryCode());
  }
}
