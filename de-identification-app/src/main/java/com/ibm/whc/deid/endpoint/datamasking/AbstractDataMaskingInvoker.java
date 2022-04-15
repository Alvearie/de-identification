/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.endpoint.datamasking;

import java.io.IOException;
import java.util.List;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.shared.exception.InvalidInputException;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;
import com.ibm.whc.deid.shared.pojo.config.GlobalProcessorConfig;

public abstract class AbstractDataMaskingInvoker {

  // Validates data is non-null and non-empty. Validates individual strings for non-null as well.
  protected void validateData(List<String> data) throws InvalidInputException {
    if (data == null || data.isEmpty()) {
      throw new InvalidInputException("data");
    }
    int i = 0;
    for (String d : data) {
      if (d == null) {
        throw new InvalidInputException("data[" + i + "]");
      }
      i++;
    }
  }

  // validates the global (a.k.a. document-level) configuration and returns it as a Java object
  protected GlobalProcessorConfig validateGlobalConfig(String gpConfigString)
      throws InvalidInputException {
    GlobalProcessorConfig gpConfig = null;
    // global processor config is optional
    if (gpConfigString != null && !gpConfigString.trim().isEmpty()) {
      try {
        gpConfig = ObjectMapperFactory.getObjectMapper().readValue(gpConfigString,
            GlobalProcessorConfig.class);
      } catch (IOException e) {
        throw new InvalidInputException("could not parse global configuration");
      }
      gpConfig.validate();
    }
    return gpConfig;
  }

  // validates the schemaType is non-null
  protected void validateSchemaType(ConfigSchemaTypes schemaType) throws InvalidInputException {
    if (schemaType == null) {
      throw new InvalidInputException("schemaType");
    }
  }

  // method called during construction of the data masking response
  protected String getString(ObjectMapper objectMapper, List<String> maskedData)
      throws IOException {
    ArrayNode aNode = objectMapper.createArrayNode();
    for (String record : maskedData) {
      aNode.add(objectMapper.readTree(record));
    }
    ObjectNode maskedNode = objectMapper.createObjectNode();
    maskedNode.set("data", aNode);
    return objectMapper.writeValueAsString(maskedNode);
  }
}
