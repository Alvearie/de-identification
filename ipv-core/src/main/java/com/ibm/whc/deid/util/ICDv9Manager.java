/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.IOException;
import java.io.InputStream;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import com.ibm.whc.deid.models.ICD;
import com.ibm.whc.deid.models.ICDFormat;
import com.ibm.whc.deid.models.ICDWithoutFormat;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import com.ibm.whc.deid.utils.log.Messages;

/**
 * Class that manages ICD version 9 codes loaded into the service.
 */
public class ICDv9Manager implements Manager {

  private static final LogManager logger = LogManager.getInstance();

  protected static final Resources resourceType = Resource.ICDV9;

  // The number of codes expected to be loaded from the resource file.
  // Modify this value if the resource file is modified.
  private static final int EXPECTED_COUNT = 15000;

  private final SecureRandom random = new SecureRandom();

  private final ArrayList<ICDWithoutFormat> icdList;
  private final HashMap<String, ICDWithoutFormat> icdByCodeMap;
  private final HashMap<String, ICDWithoutFormat> icdByNameMap;
  private final HashMap<String, ICDWithoutFormat> icdByShortMap;

  protected ICDv9Manager() {
    icdList = new ArrayList<>(EXPECTED_COUNT);
    int hashMapInitialSize = Math.round(EXPECTED_COUNT / 0.75f) + 1;
    icdByCodeMap = new HashMap<>(hashMapInitialSize, 0.75f);
    icdByNameMap = new HashMap<>(hashMapInitialSize, 0.75f);
    icdByShortMap = new HashMap<>(hashMapInitialSize, 0.75f);
  }

  /**
   * Creates a new ICDv10Manager instance from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a ICDv10Manager instance
   * 
   * @see LocalizationManager
   */
  public static ICDv9Manager buildICDv9Manager(String localizationProperty) {
    ICDv9Manager manager = new ICDv9Manager();

    try {
      Collection<ResourceEntry> resourceEntries =
          LocalizationManager.getInstance(localizationProperty).getResources(resourceType);
      for (ResourceEntry entry : resourceEntries) {

        try (InputStream inputStream = entry.createStream()) {
          String fileName = entry.getFilename();

          try (CSVParser parser = Readers.createCSVReaderFromStream(inputStream, ';', '"')) {
            for (CSVRecord record : parser) {
              loadCSVRecord(fileName, manager, record);
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
  protected static void loadCSVRecord(String fileName, ICDv9Manager manager, CSVRecord record) {
    try {
      loadRecord(manager, record.get(0), record.get(1), record.get(2), record.get(3), record.get(4),
          record.get(5), record.get(6));

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
  protected static void loadRecord(ICDv9Manager manager, String... record) {
    String code = record[0];
    String shortName = record[1];
    String fullName = record[2];
    String chapterCode = record[3];
    String chapterName = record[4];
    String categoryCode = record[5];
    String categoryName = record[6];

    manager.add(new ICDWithoutFormat(code, shortName, fullName, chapterCode, chapterName,
        categoryCode, categoryName));
  }

  protected void add(ICDWithoutFormat icdCode) {
    icdList.add(icdCode);
    icdByCodeMap.put(icdCode.getCode().toUpperCase(), icdCode);
    icdByNameMap.put(icdCode.getFullName().toUpperCase(), icdCode);
    icdByShortMap.put(icdCode.getShortName().toUpperCase(), icdCode);
  }

  public String getRandomValue(ICDFormat format) {
    String value = null;
    ICDWithoutFormat icd = null;
    int count = this.icdList.size();
    if (count == 1) {
      icd = this.icdList.get(0);
    } else if (count > 1) {
      icd = this.icdList.get(random.nextInt(count));
    }
    if (icd != null) {
      value = format == ICDFormat.NAME ? icd.getFullName() : icd.getCode();
    }
    return value;
  }

  /**
   * Retrieve an ICD code by its code or name.
   *
   * @param codeOrName the code or name
   * @return the corresponding ICD code or <i>null</i> if no such code is loaded.
   */
  public ICD lookupICD(String codeOrName) {
    if (codeOrName != null) {
      String key = codeOrName.toUpperCase();

      ICDWithoutFormat icd = this.icdByCodeMap.get(key);
      if (icd != null) {
        return new ICD(icd, ICDFormat.CODE);
      }

      icd = this.icdByNameMap.get(key);
      if (icd != null) {
        return new ICD(icd, ICDFormat.NAME);
      }

      icd = this.icdByShortMap.get(key);
      if (icd != null) {
        return new ICD(icd, ICDFormat.NAME);
      }
    }
    return null;
  }

  @Override
  public boolean isValidKey(String codeOrName) {
    return lookupICD(codeOrName) != null;
  }
}
