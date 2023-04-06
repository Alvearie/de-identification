/*
 * © Merative US L.P. 2016,2021
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
          String fileName = entry.getFilename();

          try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
            for (CSVRecord line : reader) {
              loadCSVRecord(fileName, manager, line);
            }
          }
        }
      }
    } catch (IOException e) {
      logger.logError(LogCodes.WPH1013E, e);
      throw new RuntimeException(e);
    }

    return manager;
  }

  /**
   * Retrieves data from the given Comma-Separated Values (CSV) record and loads it into the given
   * resource manager.
   *
   * @param fileName the name of the file from which the CSV data was obtained - used for logging
   *        and error messages
   * @param manager the resource manager
   * @param record a single record read from a source that provides CSV format data
   * 
   * @throws RuntimeException if any of the data in the record is invalid for its target purpose.
   */
  protected static void loadCSVRecord(String fileName, PostalCodeManager manager,
      CSVRecord record) {
    try {
      loadRecord(manager, record.get(0), record.get(1), record.get(2), record.get(3));

    } catch (RuntimeException e) {
      // CSVRecord has a very descriptive toString() implementation
      String logmsg =
          Messages.getMessage(LogCodes.WPH1023E, String.valueOf(record), fileName, e.getMessage());
      throw new KeyedRuntimeException(LogCodes.WPH1023E, logmsg, e);
    }
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
    // TODO: take advantage of country code in column 0, which is currently unused
    String code = record[1];
    String latitude = record[2];
    String longitude = record[3];
    PostalCode postalCode = new PostalCode(code, latitude, longitude);
    manager.add(postalCode);
  }

  /**
   * Gets closest postal codes.
   *
   * @param postalCode the postal code identifier
   * @param k the maximum number of the closest postal codes to return
   * @return the non-null, possibly-empty list of up to the given number of closest known postal
   *         codes
   */
  public List<PostalCode> getClosestPostalCodes(String postalCode, int k) {
    PostalCode lookup = getValue(postalCode);
    if (lookup == null || lookup.getLocation() == null) {
      return new ArrayList<>();
    }
    LatLonDistance<PostalCode> calc = new LatLonDistance<>(getValues());
    return calc.findNearestK(lookup.getLocation(), k);
  }
}
