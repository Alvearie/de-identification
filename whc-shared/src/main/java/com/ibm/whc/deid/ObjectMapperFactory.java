/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.pojo.config.DeidConfig;
import com.ibm.whc.deid.shared.util.ConfigUtil;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

// This class is a utility class to retrieve an ObjectMapper with the correct
// serialization/deserialization modules set up
public class ObjectMapperFactory {

  private static LogManager log = LogManager.getInstance();
  protected static ObjectMapper objectMapper = null;

  public static ObjectMapper getObjectMapper() {
    // ok, if we have a race condition and end up creating multiple objectMappers
    if (objectMapper == null) {
      try {
        DeidConfig deidConfig = ConfigUtil.getDeidConfig();

        @SuppressWarnings("unchecked")
        Constructor<? extends ObjectMapperUtil> constructor =
            (Constructor<? extends ObjectMapperUtil>) Class
                .forName(deidConfig.getObjectMapperUtilClass()).getConstructor();
        ObjectMapperUtil util = constructor.newInstance();
        objectMapper = util.createNewObjectMapper();
      } catch (NoSuchMethodException | ClassNotFoundException | IllegalAccessException
          | InstantiationException | InvocationTargetException | DeidException e) {
        log.logError(LogCodes.WPH1013E, e);
        throw new RuntimeException(e.getMessage());
      }
    }
    return objectMapper;
  }

}
