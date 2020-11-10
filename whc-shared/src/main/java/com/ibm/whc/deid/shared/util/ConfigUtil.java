/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.util;

import java.io.IOException;
import java.io.InputStream;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.pojo.config.DeidConfig;

/*
 * Utility class to retrieve DeidConfig configuration
 */
public class ConfigUtil {

  public static DeidConfig getDeidConfig() throws DeidException {
    ObjectMapper mapper = new ObjectMapper(new YAMLFactory());
    DeidConfig config;

    try (InputStream is = ConfigUtil.class.getClassLoader().getResourceAsStream("deid.yaml")) {
      if (is == null) {
        return new DeidConfig();
      }
      config = mapper.readValue(is, DeidConfig.class);
    } catch (IOException e) {
      throw new DeidException("Unable to read the config file", e);
    }

    return config;
  }

}
