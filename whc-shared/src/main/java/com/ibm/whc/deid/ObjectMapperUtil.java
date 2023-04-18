/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;

// This class is a utility class to retrieve an ObjectMapper with the correct
// serialization/deserialization modules set up
public class ObjectMapperUtil {

  public ObjectMapper createNewObjectMapper() {
    ObjectMapper objectMapperInProgress = new ObjectMapper();
    SimpleModule module = new SimpleModule();
    module.addDeserializer(ConfigSchemaTypes.class, new ConfigSchemaTypesDeserializer());
    objectMapperInProgress.registerModule(module);
    return objectMapperInProgress;
  }

}
