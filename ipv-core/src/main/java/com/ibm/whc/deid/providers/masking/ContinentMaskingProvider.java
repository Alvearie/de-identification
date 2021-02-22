/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.util.Map;

import com.ibm.whc.deid.models.City;
import com.ibm.whc.deid.models.Continent;
import com.ibm.whc.deid.models.Country;
import com.ibm.whc.deid.models.OriginalMaskedValuePair;
import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.schema.FieldRelationship;
import com.ibm.whc.deid.schema.RelationshipOperand;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.ContinentMaskingProviderConfig;
import com.ibm.whc.deid.util.CityManager;
import com.ibm.whc.deid.util.ContinentManager;
import com.ibm.whc.deid.util.CountryManager;
import com.ibm.whc.deid.util.ManagerFactory;

/**
 * The type Continent masking provider.
 *
 */
public class ContinentMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -4463664508295286291L;

  protected CountryManager countryManager;
  protected CityManager cityManager;
  protected final boolean getClosest;
  protected final int getClosestK;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  protected ContinentManager continentManager;

  protected volatile boolean initialized = false;

  public ContinentMaskingProvider(ContinentMaskingProviderConfig configuration, String tenantId, String localizationProperty) {
    this.getClosest = configuration.isMaskClosest();
    this.getClosestK = configuration.getMaskClosestK();
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
		this.localizationProperty = localizationProperty;
  }

  protected void initialize() {
    if (!initialized) {
      continentManager = (ContinentManager) ManagerFactory.getInstance().getManager(null,
          Resource.CONTINENT, null, localizationProperty);

      countryManager =
          (CountryManager) ManagerFactory.getInstance().getManager(null, Resource.COUNTRY, null, localizationProperty);

      cityManager =
          (CityManager) ManagerFactory.getInstance().getManager(null, Resource.CITY, null, localizationProperty);

      initialized = true;
    }
  }

  @Override
  public String mask(String identifier, String fieldName, FieldRelationship fieldRelationship,
      Map<String, OriginalMaskedValuePair> values) {

    initialize();
    if (identifier == null) {
      return null;
    }

    RelationshipOperand relationshipOperand = fieldRelationship.getOperands()[0];

    String operandFieldName = relationshipOperand.getName();
    String operandMaskedValue = values.get(operandFieldName).getMasked();

    if (relationshipOperand.getType() == ProviderType.COUNTRY) {
      Continent continent = continentManager.getKey(identifier);
      String locale = null;
      if (continent != null) {
        locale = continent.getNameCountryCode();
      }

      Country country = countryManager.lookupCountry(operandMaskedValue, locale);
      if (country == null) {
        return mask(identifier);
      }
      return country.getContinent();

    } else if (relationshipOperand.getType() == ProviderType.CITY) {
      City city = cityManager.getKey(operandMaskedValue);
      if (city != null) {
        String countryCode = city.getCountryCode();
        Country country = countryManager.lookupCountry(countryCode, city.getNameCountryCode());
        if (country != null) {
          return country.getContinent();
        }
      }
      return mask(identifier);
    }

    return mask(identifier);
  }

  @Override
  public String mask(String identifier) {
    initialize();
    try {
      if (identifier == null) {
        debugFaultyInput("identifier");
        return null;
      }

      Continent continent = continentManager.getKey(identifier);
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
