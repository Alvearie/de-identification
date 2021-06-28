/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.util.concurrent.ConcurrentHashMap;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;

/**
 * Class that provides instances of resource managers of all supported types.
 */
public class ManagerFactory {

  private static final ManagerFactory instance = new ManagerFactory();

  /**
   * Resource manager takes time to instantiate. To save time, we cache resource managers.
   */
  private final ConcurrentHashMap<String, Manager> managers = new ConcurrentHashMap<>();

  private ManagerFactory() {}

  public static ManagerFactory getInstance() {
    return instance;
  }

  /**
   * Get a resource based manager. First check to see if there is a cached version, otherwise create
   * a new one.
   *
   * @param tenantId
   * @param resourceType
   * @param options optional options a manager might have. eg zipcode manager's prefixLength
   * @paramlocalizationProperty location of the localization property file
   * @return
   */
  public Manager getManager(String tenantId, Resources resourceType, Object options,
      String localizationProperty) {
    String cacheKey = resourceType + "_" + localizationProperty;
    Manager manager = managers.get(cacheKey);
    if (manager != null) {
      return manager;
    }

    if (resourceType instanceof Resource) {
      switch ((Resource) resourceType) {
        case ATC_CODES:
          manager = ATCManager.buildATCManager(localizationProperty);
          break;
        case CITY:
          manager = CityManager.buildCityManager(localizationProperty);
          break;
        case POSTAL_CODES:
          manager = PostalCodeManager.buildPostalCodeManager(localizationProperty);
          break;
        case CONTINENT:
          manager = ContinentManager.buildContinentManager(localizationProperty);
          break;
        case COUNTRY:
          manager = CountryManager.buildCountryManager(localizationProperty);
          break;
        case COUNTY:
          manager = CountyManager.buildCountyManager(localizationProperty);
          break;
        case CREDIT_CARD_TYPE:
          manager = CreditCardTypeManager.buildCreditCardTypeManager(localizationProperty);
          break;
        case FIRST_NAME_FEMALE:
          manager = NameFirstFemaleManager.buildNameFirstFemaleManager(localizationProperty);
          break;
        case FIRST_NAME_MALE:
          manager = NameFirstMaleManager.buildNameFirstMaleManager(localizationProperty);
          break;
        case GENDER:
          manager = GenderManager.buildGenderManager(localizationProperty);
          break;
        case HOSPITAL_NAMES:
          manager = HospitalManager.buildHospitalManager(localizationProperty);
          break;
        case ICDV10:
          manager = ICDv10Manager.buildICDv10Manager(localizationProperty);
          break;
        case ICDV9:
          manager = ICDv9Manager.buildICDv9Manager(localizationProperty);
          break;
        case TACDB:
          manager = IMEIManager.buildIMEIManager(localizationProperty);
          break;
        case LAST_NAME:
          manager = NameLastManager.buildNameLastManager(localizationProperty);
          break;
        case MARITAL_STATUS:
          manager = MaritalStatusManager.buildMaritalStatusManager(localizationProperty);
          break;
        case OCCUPATION:
          manager = OccupationManager.buildOccupationManager(localizationProperty);
          break;
        case PHONE_AREA_CODES:
          manager = PhoneAreaCodesManager.buildPhoneAreaCodesManager(localizationProperty);
          break;
        case PHONE_CALLING_CODES:
          manager = PhoneCountryCodesManager.buildPhoneCountryCodesManager(localizationProperty);
          break;
        case PHONE_NUM_DIGITS:
          manager = PhoneNumberLengthManager.buildPhoneNumberLengthManager(localizationProperty);
          break;
        case RACE_ETHNICITY:
          manager = RaceManager.buildRaceManager(localizationProperty);
          break;
        case RELIGION:
          manager = ReligionManager.buildReligionManager(localizationProperty);
          break;
        case STREET_NAMES:
          manager = StreetNameManager.buildStreetNameManager(localizationProperty);
          break;
        case STATES_US:
          manager = StatesUSManager.buildStatesUSManager(localizationProperty);
          break;
        case SWIFT:
          manager = SWIFTCodeManager.buildSWIFTCodeManager(localizationProperty);
          break;
        case WORLD_MANUFACTURERS_IDENTIFIER:
          manager = VINManager.buildVINManager(localizationProperty);
          break;
        case ZIPCODE:
          manager = ZIPCodeManager.buildZIPCodeManager(localizationProperty);
          break;
        default:
          throw new IllegalArgumentException("Unsupported resource type:" + resourceType);
      }
    } else {
      throw new IllegalArgumentException("Unsupported resource type:" + resourceType);
    }

    managers.put(cacheKey, manager);

    return manager;
  }
}
