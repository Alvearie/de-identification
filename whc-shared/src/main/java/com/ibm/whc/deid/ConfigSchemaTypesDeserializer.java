/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid;

import java.io.IOException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;

// This class is used to help deserialize the interface into the ConfigSchemaType enum
// It is registered in the ObjectMapper retrieved via ObjectMapperFactory.getObjectMapper
public class ConfigSchemaTypesDeserializer extends StdDeserializer<ConfigSchemaTypes> {

  private static final long serialVersionUID = 860481513479439201L;

  public ConfigSchemaTypesDeserializer() {
    this(null);
  }

  protected ConfigSchemaTypesDeserializer(Class<?> vc) {
    super(vc);
  }

  private static ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();

  @Override
  public ConfigSchemaTypes deserialize(JsonParser p, DeserializationContext ctxt)
      throws IOException, JsonProcessingException {
    return objectMapper.readValue(p, ConfigSchemaType.class);
  }
}