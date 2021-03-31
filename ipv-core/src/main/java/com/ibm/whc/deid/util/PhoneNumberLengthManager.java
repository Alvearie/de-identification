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
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

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
    // nothing required here
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

    Collection<ResourceEntry> resourceEntries =
        LocalizationManager.getInstance(localizationProperty).getResources(Resource.PHONE_NUM_DIGITS);
    
    for (ResourceEntry entry : resourceEntries) {
      try (InputStream inputStream = entry.createStream()) {

        try (CSVParser reader = Readers.createCSVReaderFromStream(inputStream)) {
          for (CSVRecord line : reader) {          
            String callingCode = line.get(0);  
            String currArrayString = line.get(1);
            
            if (callingCode.isEmpty() || currArrayString.isEmpty()) {
              continue;
            }
            List<Integer> currList = convertStringValueToList(currArrayString);
            if (currList == null || currList.isEmpty()) {
              continue;
            }
            
            KeyListResource<Integer> resource = new KeyListResource<>(callingCode, currList);
            mgr.add(resource);
          }
        }
        
      } catch (IOException | NullPointerException e) {
        logger.logError(LogCodes.WPH2003E,
            "Failed to load telephone number length list " + " for tenant ", e);
      }
    }

    return mgr;
  }
  
  protected static List<Integer> convertStringValueToList(String currArrayString) {
    List<Integer> currList = new ArrayList<>();
    if (currArrayString.contains("-")) {
      String[] currArray = currArrayString.split("-");
      if (currArray.length == 2) {
        int startNum;
        try {
          startNum = Integer.parseInt(currArray[0]);
        } catch (NumberFormatException e) {
          logger.logError(LogCodes.WPH2003E,
              "Ignoring non-numeric data in telephone number length list: " + currArray[0]);
          return null;
        }
        int endNum;
        try {
          endNum = Integer.parseInt(currArray[1]);
        } catch (NumberFormatException e) {
          logger.logError(LogCodes.WPH2003E,
              "Ignoring non-numeric data in telephone number length list: " + currArray[1]);
          return null;
        }
        for (int idx = startNum; idx <= endNum; idx++) {
          currList.add(Integer.valueOf(idx));
        }
      }
    } else if (currArrayString.contains("|")) {
      String[] currArray = currArrayString.split("\\|");
      for (int idx = 0; idx < currArray.length; idx++) {
        try {
          currList.add(Integer.valueOf(currArray[idx]));
        } catch (NumberFormatException e) {
          logger.logError(LogCodes.WPH2003E,
              "Ignoring non-numeric data in telephone number length list: " + currArray[idx]);
          return null;
        }
      }
    } else {
      try {
        currList.add(Integer.valueOf(currArrayString));
      } catch (NumberFormatException e) {
        logger.logError(LogCodes.WPH2003E,
            "Ignoring non-numeric data in telephone number length list: " + currArrayString);
        return null;
      }
    }
    return currList;
  }
}
