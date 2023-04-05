/*
 * © Merative US L.P. 2023
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.util;

import java.io.IOException;
import java.io.InputStream;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.pojo.config.DeidConfig;

/*
 * Utility class to retrieve DeidConfig configuration
 */
public class ConfigUtil {

  public static DeidConfig getDeidConfig() throws DeidException {
    // use a plain object mapper -
    // no special configuration is necessary for the simple json file
    ObjectMapper mapper = new ObjectMapper();
    DeidConfig config;

    // up through version 1.2.0, this file was a yaml file - deid.yaml
    try (InputStream is = ConfigUtil.class.getClassLoader().getResourceAsStream("deid.config.json")) {
      if (is == null) {
        config = new DeidConfig();
      } else {
        config = mapper.readValue(is, DeidConfig.class);
      }
    } catch (IOException e) {
      throw new DeidException("Unable to read the config file", e);
    }
    
    return config;
  }
}
