/*
 * © Merative US L.P. 2016, 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * The model class which represents the post data for De-Identification REST API
 */
@Schema(name = "dataMaskingModel", description = "De-identification input object")
public class DataMaskingModel {

  @Schema(description = "Masking configuration")
  protected final String config;

  @Schema(description = "A list of data to be masked")
  protected final List<String> data;

  @Schema(description = "The format of the data to be masked", implementation = String.class)
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
