/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.LatitudeLongitude;
import com.ibm.whc.deid.models.PostalCode;
import com.ibm.whc.deid.resources.ResourceManager;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class PostalCodeManager extends ResourceManager<PostalCode> {

  private static LogManager logger = LogManager.getInstance();

  protected static final Resources resourceType = Resource.POSTAL_CODES;

  protected LatLonDistance<PostalCode> latLonTree = null;

  protected PostalCodeManager() {
    super(44000);
  }

  /**
   * Creates a new PostalCodeManager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a PostalCodeManager instance
   * 
   * @see LocalizationManager
   */
  public static PostalCodeManager buildPostalCodeManager(String localizationProperty) {
    PostalCodeManager manager = new PostalCodeManager();

    Collection<ResourceEntry> resourceEntries =
        LocalizationManager.getInstance(localizationProperty).getResources(resourceType);
    for (ResourceEntry entry : resourceEntries) {

      try (InputStream inputStream = entry.createStream()) {
        String locale = entry.getCountryCode();

        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord line : reader) {
            // column 0 skipped
            String code = line.get(1);
            if (!code.trim().isEmpty()) {
              Double latitude = FileUtils.parseDouble(line.get(2));
              Double longitude = FileUtils.parseDouble(line.get(3));
              // TODO : replace hardcoded locale
              PostalCode postalCode = new PostalCode(code, latitude, longitude);
              manager.add(postalCode);
            }
          }
        }

      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH1013E, e);
      }
    }

    manager.latLonTree = new LatLonDistance<>(manager.getValues());

    return manager;
  }

  /**
   * Gets closest postal codes.
   *
   * @param postalCode the postal code
   * @param k the k
   * @return the closest postal codes
   */
  public List<PostalCode> getClosestPostalCodes(String postalCode, int k) {
    PostalCode lookup = getValue(postalCode);

    if (lookup == null) {
      return new ArrayList<>();
    }

    LatitudeLongitude latlon = lookup.getLocation();

    return this.latLonTree.findNearestK(latlon.getLatitude(), latlon.getLongitude(), k);
  }
}
