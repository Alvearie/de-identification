/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.util.concurrent.ConcurrentHashMap;

import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;

/**
 * Resource manager takes time to instantiate. To save time, we cache resource
 * managers.
 *
 */
public class ManagerFactory {

  private static final ManagerFactory instance = new ManagerFactory();

	ConcurrentHashMap<String, Manager> managers = new ConcurrentHashMap<>();

	private ManagerFactory() {
	}

  public static ManagerFactory getInstance() {
    return instance;
  }

	/**
	 * Get a resource based manager. First check to see if there is a cached
	 * version, otherwise create a new one
	 *
	 * @param tenantId
	 * @param resourceType
	 * @param options      optional options a manager might have. eg zipcode
	 *                     manager's prefixLength
	 * @param localizationProperty TODO
	 * @return
	 */
  public Manager getManager(String tenantId, Resources resourceType, Object options, String localizationProperty) {

		// The manager needs to be tenant specific based on type
		String key = tenantId + "_" + resourceType;
		Manager manager = managers.get(key);

    if (manager != null) {
      // check prefix length for ZIPCodeManager. Only return the cached
      // version if the prefixLength is the same
      if (resourceType == Resource.ZIPCODE) {
        int newPrefixLength = (int) options;
        int cachedPrefixLength = ((ZIPCodeManager) manager).getPrefixLength();
        if (newPrefixLength == cachedPrefixLength) {
          return manager;
        }
      } else {
        return manager;
      }
    }

    if (resourceType instanceof Resource) {
      switch ((Resource) resourceType) {
        case ATC_CODES:
				manager = new ATCManager(tenantId, localizationProperty);
          break;
        case CITY:
				manager = new CityManager(tenantId, localizationProperty);
          break;
        case POSTAL_CODES:
          manager = new PostalCodeManager(tenantId, localizationProperty);
          break;
        case CONTINENT:
          manager = new ContinentManager(tenantId, localizationProperty);
          break;
        case COUNTRY:
          manager = new CountryManager(tenantId, localizationProperty);
          break;
        case COUNTY:
          manager = new CountyManager(tenantId, localizationProperty);
          break;
        case GENDER:
          manager = new GenderManager(tenantId, localizationProperty);
          break;
        case HOSPITAL_NAMES:
          manager = new HospitalManager(tenantId, localizationProperty);
          break;
        case ICDV10:
          manager = new ICDv10Manager(tenantId, localizationProperty);
          break;
        case ICDV9:
          manager = new ICDv9Manager(tenantId, localizationProperty);
          break;
        case MARITAL_STATUS:
          manager = new MaritalStatusManager(tenantId, localizationProperty);
          break;
        case OCCUPATION:
          manager = new OccupationManager(tenantId, localizationProperty);
          break;
        case RACE_ETHNICITY:
          manager = new RaceManager(tenantId, localizationProperty);
          break;
        case RELIGION:
          manager = new ReligionManager(tenantId, localizationProperty);
          break;
        case STREET_NAMES:
          manager = new StreetNameManager(tenantId, localizationProperty);
          break;
        case STATES_US:
          manager = new StatesUSManager(tenantId, localizationProperty);
          break;
        case SWIFT:
          manager = new SWIFTCodeManager(tenantId, localizationProperty);
          break;
        case WORLD_MANUFACTURERS_IDENTIFIER:
				manager = new VINManager(tenantId, localizationProperty);
          break;
        case ZIPCODE:
          int prefixLength = (int) options;
          manager = new ZIPCodeManager(prefixLength, tenantId, localizationProperty);
          break;
        default:
          throw new IllegalArgumentException("Unsupported resource type:" + resourceType);
      }
    } else {
      throw new IllegalArgumentException("Unsupported resource type:" + resourceType);
    }

		managers.put(key, manager);

    return manager;
  }

}
