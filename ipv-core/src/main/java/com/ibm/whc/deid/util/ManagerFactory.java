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

  ConcurrentHashMap<Resources, Manager> managers = new ConcurrentHashMap<>();

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
	 * @return
	 */
  public Manager getManager(String tenantId, Resources resourceType, Object options) {

    Manager manager = managers.get(resourceType);

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
          manager = new ATCManager(tenantId);
          break;
        case CITY:
          manager = new CityManager(tenantId);
          break;
        case POSTAL_CODES:
          manager = new PostalCodeManager(tenantId);
          break;
        case CONTINENT:
          manager = new ContinentManager(tenantId);
          break;
        case COUNTRY:
          manager = new CountryManager(tenantId);
          break;
        case COUNTY:
          manager = new CountyManager(tenantId);
          break;
        case GENDER:
          manager = new GenderManager(tenantId);
          break;
        case HOSPITAL_NAMES:
          manager = new HospitalManager(tenantId);
          break;
        case ICDV10:
          manager = new ICDv10Manager(tenantId);
          break;
        case ICDV9:
          manager = new ICDv9Manager(tenantId);
          break;
        case MARITAL_STATUS:
          manager = new MaritalStatusManager(tenantId);
          break;
        case OCCUPATION:
          manager = new OccupationManager(tenantId);
          break;
        case RACE_ETHNICITY:
          manager = new RaceManager(tenantId);
          break;
        case RELIGION:
          manager = new ReligionManager(tenantId);
          break;
        case STREET_NAMES:
          manager = new StreetNameManager(tenantId);
          break;
        case STATES_US:
          manager = new StatesUSManager(tenantId);
          break;
        case SWIFT:
          manager = new SWIFTCodeManager(tenantId);
          break;
        case WORLD_MANUFACTURERS_IDENTIFIER:
          manager = new VINManager(tenantId);
          break;
        case ZIPCODE:
          int prefixLength = (int) options;
          manager = new ZIPCodeManager(prefixLength, tenantId);
          break;
        default:
          throw new IllegalArgumentException("Unsupported resource type:" + resourceType);
      }
    } else {
      throw new IllegalArgumentException("Unsupported resource type:" + resourceType);
    }

    managers.put(resourceType, manager);

    return manager;
  }

}
