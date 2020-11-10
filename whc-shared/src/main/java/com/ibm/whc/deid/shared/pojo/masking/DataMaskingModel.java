/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;

/*
 * The model class which represents the post data for deidentification rest api
 */
public class DataMaskingModel {

  protected final String config;

  protected final List<String> data;

  protected final ConfigSchemaTypes schemaType;

  @JsonCreator(mode = JsonCreator.Mode.PROPERTIES)
  public DataMaskingModel(@JsonProperty("config") String config,
      @JsonProperty("data") List<String> data,
      @JsonProperty("schemaType") ConfigSchemaTypes schemaType) {
    this.config = config;
    this.data = data;
    this.schemaType = schemaType;
  }

  public String getConfig() {
    return config;
  }

  public List<String> getData() {
    return data;
  }

  public ConfigSchemaTypes getSchemaType() {
    return schemaType;
  }
}
