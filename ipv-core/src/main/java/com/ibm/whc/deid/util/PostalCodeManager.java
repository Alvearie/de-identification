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
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import com.ibm.whc.deid.utils.log.Messages;

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

    try {
      Collection<ResourceEntry> resourceEntries =
          LocalizationManager.getInstance(localizationProperty).getResources(resourceType);
      for (ResourceEntry entry : resourceEntries) {

        try (InputStream inputStream = entry.createStream()) {
          // TODO: take advantage of locale
          // String locale = entry.getCountryCode();

          try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
            for (CSVRecord line : reader) {
              try {
                // column 0 skipped
                loadRecord(manager, line.get(1), line.get(2), line.get(3));

              } catch (RuntimeException e) {
                // CSVRecord has a very descriptive toString() implementation
                String logmsg = Messages.getMessage(LogCodes.WPH1023E, String.valueOf(line),
                    entry.getFilename(), e.getMessage());
                throw new KeyedRuntimeException(LogCodes.WPH1023E, logmsg, e);
              }
            }
          }
        }
      }
    } catch (IOException e) {
      logger.logError(LogCodes.WPH1013E, e);
      throw new RuntimeException(e);
    }

    manager.latLonTree = new LatLonDistance<>(manager.getValues());

    return manager;
  }

  /**
   * Retrieves data from the given record and loads it into the given resource manager.
   *
   * @param manager the resource manager
   * @param record the data from an input record to be loaded as resources into the manager
   * 
   * @throws RuntimeException if any of the data in the record is invalid for its target purpose.
   */
  protected static void loadRecord(PostalCodeManager manager, String... record) {
    String code = record[0];
    String latitude = record[1];
    String longitude = record[2];
    PostalCode postalCode = new PostalCode(code, latitude, longitude);
    manager.add(postalCode);
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
