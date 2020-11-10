/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util.localization;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class ResourceEntry {
  private final String filename;
  private final String countryCode;
  private final ResourceEntryType resourceEntryType;

  private static final LogManager logger = LogManager.getInstance();

  /**
   * Instantiates a new Resource entry.
   *
   * @param filename the filename
   * @param countryCode the country code
   * @param resourceEntryType the resource entry type
   */
  public ResourceEntry(String filename, String countryCode, ResourceEntryType resourceEntryType) {
    this.filename = filename;
    this.countryCode = countryCode;
    this.resourceEntryType = resourceEntryType;
  }

  /**
   * Create stream input stream.
   *
   * @return the input stream
   */
  public InputStream createStream() {
    // logger.info("Creating stream for {} {}", filename, countryCode);

    switch (resourceEntryType) {
      case INTERNAL_RESOURCE:
        // logger.debug("Returning internal resource");
        return this.getClass().getResourceAsStream(filename);
      case EXTERNAL_FILENAME:
        try {
          // logger.debug("Returning ");
          return new FileInputStream(filename);
        } catch (FileNotFoundException e) {
          logger.logError(LogCodes.WPH1013E, e);
          throw new RuntimeException("Cannot create FileInputStream for file", e);
        }
      default:
        // logger.debug("Returning null, something went very wrong");
        return null;
    }
  }

  /**
   * Gets country code.
   *
   * @return the country code
   */
  public String getCountryCode() {
    return countryCode;
  }
}
