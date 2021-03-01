/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import java.util.List;
import com.ibm.whc.deid.models.Address;
import com.ibm.whc.deid.models.PostalCode;
import com.ibm.whc.deid.models.RoadTypes;
import com.ibm.whc.deid.providers.identifiers.AddressIdentifier;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.AddressMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.util.HashUtils;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.PostalCodeManager;
import com.ibm.whc.deid.util.StreetNameManager;

/**
 * The type Address masking provider.
 *
 */
public class AddressMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -7633320767310691175L;

  private static final AddressIdentifier addressIdentifier = new AddressIdentifier();
  protected PostalCodeManager postalCodeManager;
  protected StreetNameManager streetNameManager;
  protected CountryMaskingProvider countryMaskingProvider;
  protected CityMaskingProvider cityMaskingProvider;
  private final boolean randomizeCountry;
  private final boolean randomizeNumber;
  private final boolean randomizeRoadType;
  private final boolean randomizePostalCode;
  private final boolean nearestPostalCode;
  private final int nearestPostalCodeK;
  private final boolean randomizeCity;
  private final boolean randomizeName;
  private final boolean getPseudorandom;
  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;

  protected final AddressMaskingProviderConfig configuration;

  protected final MaskingProviderFactory maskingProviderFactory;

  protected volatile boolean initialized = false;

  public AddressMaskingProvider(AddressMaskingProviderConfig configuration, String tenantId,
      MaskingProviderFactory maskingProviderFactory, String localizationProperty) {

    super(tenantId, localizationProperty);

    this.maskingProviderFactory = maskingProviderFactory;

    this.configuration = configuration;
    this.random = new SecureRandom();
    this.getPseudorandom = configuration.isMaskPseudorandom();
    if (this.getPseudorandom) {
      configuration.setCountryMaskPseudorandom(true);
      configuration.setCityMaskPseudorandom(true);
    }

    this.randomizeCountry = configuration.isCountryMask();
    this.randomizeNumber = configuration.isNumberMask();
    this.randomizeRoadType = configuration.isRoadTypeMask();
    this.randomizePostalCode = configuration.isPostalCodeMask();
    this.nearestPostalCode = configuration.isPostalCodeNearest();
    this.nearestPostalCodeK = configuration.getPostalCodeNearestK();
    this.randomizeCity = configuration.isCityMask();
    this.randomizeName = configuration.isStreetNameMask();

    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  @Override
  public String mask(String identifier) {
    initialize();
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    Address randomAddress;

    Address address = addressIdentifier.parseAddress(identifier);
    if (address == null) {
      debugFaultyInput("address");
      if (unspecifiedValueHandling == 2) {
        address = new Address("", "", "", "", "", "");
        randomAddress = new Address();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    } else {
      randomAddress = new Address(address);
    }

    if (address.isPOBox()) {
      randomAddress.setPoBox(true);

      if (this.getPseudorandom) {
        String poBoxNumber = address.getPoBoxNumber();
        randomAddress
            .setPoBoxNumber(Long.toString(Math.abs(HashUtils.longFromHash(poBoxNumber)) % 10000));
      } else {
        randomAddress.setPoBoxNumber(random.nextInt(10000) + "");
      }
      return randomAddress.toString();
    }

    if (this.randomizeNumber) {
      if (this.getPseudorandom) {
        String number = randomAddress.getNumber();
        randomAddress.setNumber(Long.toString(Math.abs(HashUtils.longFromHash(number)) % 300));
      } else {
        randomAddress.setNumber(this.random.nextInt(300) + "");
      }
    }

    if (this.randomizeCity) {
      // psuedorandom is embedded into the provider itself, we have
      // set the configuration accordingly
      randomAddress.setCityOrState(cityMaskingProvider.mask(address.getCityOrState()));
    }

    if (this.randomizeCountry) {
      // psuedorandom is embedded into the provider itself, we have
      // set the configuration accordingly
      randomAddress.setCountry(countryMaskingProvider.mask(address.getCountry()));
    }

    if (this.randomizeName) {
      if (this.getPseudorandom) {
        String sname = randomAddress.getName();
        randomAddress.setName(streetNameManager.getPseudorandom(sname));
      } else {
        randomAddress.setName(streetNameManager.getRandomKey());
      }
    }

    if (this.randomizeRoadType) {
      RoadTypes[] roadTypes = RoadTypes.values();
      int randomPosition;

      if (this.getPseudorandom) {
        randomPosition = (int) (Math.abs(HashUtils.longFromHash(identifier)) % roadTypes.length);
      } else {
        randomPosition = random.nextInt(roadTypes.length);
      }

      String randomRoadType = roadTypes[randomPosition].name();
      randomAddress.setRoadType(randomRoadType);
    }

    if (this.randomizePostalCode) {

      if (this.getPseudorandom) {
        String postalCode = address.getPostalCode();
        randomAddress.setPostalCode(postalCodeManager.getPseudorandom(postalCode));
      } else if (this.nearestPostalCode) {
        String postalCode = address.getPostalCode();
        List<PostalCode> nearest =
            postalCodeManager.getClosestPostalCodes(postalCode, this.nearestPostalCodeK);
        String replacementCode;
        if (nearest.size() > 0) {
          PostalCode replacementPostalCode = nearest.get(random.nextInt(nearest.size()));
          replacementCode = replacementPostalCode.getName();
        } else {
          replacementCode = postalCodeManager.getRandomKey();
        }

        randomAddress.setPostalCode(replacementCode);
      } else {
        randomAddress.setPostalCode(postalCodeManager.getRandomKey());
      }
    }

    return randomAddress.toString();
  }

  protected void initialize() {
    if (!initialized) {
      // Initialize all the masking providers/managers needed.
      streetNameManager = (StreetNameManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.STREET_NAMES, null, localizationProperty);

      countryMaskingProvider = (CountryMaskingProvider) maskingProviderFactory.getProviderFromType(
          MaskingProviderType.COUNTRY, null, configuration.getCountryMaskingConfig(), tenantId,
          localizationProperty);
      countryMaskingProvider.initialize();

      cityMaskingProvider =
          (CityMaskingProvider) maskingProviderFactory.getProviderFromType(MaskingProviderType.CITY,
              tenantId, configuration.getCityMaskingConfig(), null, localizationProperty);

      postalCodeManager = (PostalCodeManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.POSTAL_CODES, null, localizationProperty);
      initialized = true;
    }
  }
}
