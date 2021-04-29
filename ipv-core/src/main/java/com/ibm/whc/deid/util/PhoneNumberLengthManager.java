/*
 * (C) Copyright IBM Corp. 2021
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
import com.ibm.whc.deid.resources.KeyListResource;
import com.ibm.whc.deid.resources.ResourceManager;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import com.ibm.whc.deid.utils.log.Messages;

/**
 * Class that provides access to information about the lengths of phone numbers  
 * in various countries. 
 * 
 * <p>The keys in this resource manager are telephone country calling codes.
 * The values are the number of digits in a valid telephone number for countries
 * that use the calling code indicated by the key.
 * 
 * <p>
 * Instances of this class are thread-safe.
 */
public class PhoneNumberLengthManager extends ResourceManager<KeyListResource<Integer>> {

  private static final LogManager logger = LogManager.getInstance();

  protected PhoneNumberLengthManager() {
    super(200);
  }

  /**
   * Creates a new telephone number length manager from the definitions in the given properties file.
   * 
   * @param localizationProperty path and file name of a properties file consumed by the
   *        LocalizationManager to find the resources for this manager instance.
   * 
   * @return a telephone country calling codes manager instance
   * 
   * @see LocalizationManager
   */
  public static PhoneNumberLengthManager buildPhoneNumberLengthManager(String localizationProperty) {
    PhoneNumberLengthManager mgr = new PhoneNumberLengthManager();

    try {
      Collection<ResourceEntry> resourceEntries = LocalizationManager
          .getInstance(localizationProperty).getResources(Resource.PHONE_NUM_DIGITS);

      for (ResourceEntry entry : resourceEntries) {
        try (InputStream inputStream = entry.createStream()) {

          try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
            for (CSVRecord line : reader) {
              try {
                loadRecord(mgr, line.get(0), line.get(1));

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

    return mgr;
  }
  
  /**
   * Retrieves data from the given record and loads it into the given resource manager.
   *
   * @param manager the resource manager
   * @param record the data from an input record to be loaded as resources into the manager
   * 
   * @throws RuntimeException if any of the data in the record is invalid for its target purpose.
   */
  protected static void loadRecord(PhoneNumberLengthManager manager, String... record) {
    String callingCode = record[0];
    String currArrayString = record[1];

    List<Integer> currList = new ArrayList<>();
    try {
      if (currArrayString.contains("-")) {
        String[] currArray = currArrayString.split("-");
        if (currArray.length == 2) {
          int startNum = Integer.parseInt(currArray[0]);
          int endNum = Integer.parseInt(currArray[1]);
          if (startNum < 1) {
            throw new IllegalArgumentException(currArrayString);
          }
          if (startNum > endNum) {
            throw new IllegalArgumentException(currArrayString);
          }
          for (int idx = startNum; idx <= endNum; idx++) {
            currList.add(Integer.valueOf(idx));
          }
        } else {
          throw new IllegalArgumentException(currArrayString);
        }
      } else if (currArrayString.contains("|")) {
        String[] currArray = currArrayString.split("\\|");
        for (int idx = 0; idx < currArray.length; idx++) {
          Integer val = Integer.valueOf(currArray[idx]);
          if (val.intValue() < 1) {
            throw new IllegalArgumentException(currArrayString);
          }
          currList.add(val);
        }
      } else {
        Integer val = Integer.valueOf(currArrayString);
        if (val.intValue() < 1) {
          throw new IllegalArgumentException(currArrayString);
        }
        currList.add(val);
      }
    } catch (RuntimeException e) {
      throw new IllegalArgumentException(Messages.getMessage(LogCodes.WPH1010E,
          String.valueOf(currArrayString), "list resource value"), e);
    }

    KeyListResource<Integer> resource = new KeyListResource<>(callingCode, currList);
    manager.add(resource);
  }
}
