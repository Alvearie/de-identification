/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util.localization;

import com.ibm.whc.deid.shared.localization.Resource;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

public class ResourceDescriptionFactory {
  /**
   * Gets description.
   *
   * @param resource the resource
   * @return the description
   */
  public static ResourceDescription getDescription(Resource resource) {
    switch (resource) {
      case RELIGION:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ','", "quote: '\"'"), "Religion",
            Collections.singletonList("religion"));
      case RACE_ETHNICITY:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ','", "quote: '\"'"), "Race/Ethnicity",
            Collections.singletonList("race"));
      case OCCUPATION:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ','", "quote: '\"'"), "Occupation",
            Arrays.asList("occupation", "category"));
      case MEDICINES:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ','", "quote: '\"'"), "Medicines",
            Arrays.asList("medicine", "category"));
      case MARITAL_STATUS:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ','", "quote: '\"'"), "Marital Status",
            Collections.singletonList("status"));
      case FIRST_NAME_MALE:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ','", "quote: '\"'"), "Male names",
            Collections.singletonList("name"));
      case FIRST_NAME_FEMALE:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ','", "quote: '\"'"), "Female names",
            Collections.singletonList("name"));
      case LAST_NAME:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ','", "quote: '\"'"), "Surnames",
            Arrays.asList("id", "name"));
      case HOSPITAL_NAMES:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ','", "quote: '\"'"), "Hospital Names",
            Arrays.asList("name", "country code"));
      case CITY:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ','", "quote: '\"'"), "Cities", Arrays.asList("name",
                "alternative name", "latitude", "longitude", "country code", "population"));
      case CONTINENT:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ','", "quote: '\"'"), "Continents",
            Arrays.asList("name", "latitude", "longitude"));
      case COUNTRY:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ','", "quote: '\"'"), "Countries",
            Arrays.asList("name", "2-letter ISO code", "3-letter ISO code", "friendly name",
                "continent name", "latitude", "longitude"));
      case CREDIT_CARD_TYPE:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ','", "quote: '\"'"), "Credit card", Arrays.asList("name",
                "list of prefixes (separated by ':')", "minimum length", "maximum length"));
      case ICDV9:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ';'", "quote: '\"'"), "ICD v9", Arrays.asList("disease code",
                "name", "name", "category code", "category name", "chapter code", "chapter name"));
      case ICDV10:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ','", "quote: '\"'"), "ICD v10", Arrays.asList("disease code",
                "name", "category code", "category name", "chapter code", "chapter name"));
      case PHONE_CALLING_CODES:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ','", "quote: '\"'"), "Phone country codes",
            Arrays.asList("code", "country name"));
      case PHONE_AREA_CODES:
        return new ResourceDescription(resource, ResourceFormat.CSV,
            Arrays.asList("delimiter: ','", "quote: '\"'"), "Phone area codes",
            Arrays.asList("3-letter ISO code", "area code", "state or province"));
      default:
        return null;
    }
  }

  /**
   * Gets descriptions.
   *
   * @return the descriptions
   */
  public static Collection<ResourceDescription> getDescriptions() {
    Collection<ResourceDescription> descriptions = new ArrayList<>();

    for (Resource resource : Resource.values()) {
      ResourceDescription description = getDescription(resource);
      if (description == null) {
        continue;
      }

      descriptions.add(description);
    }

    return descriptions;
  }
}
