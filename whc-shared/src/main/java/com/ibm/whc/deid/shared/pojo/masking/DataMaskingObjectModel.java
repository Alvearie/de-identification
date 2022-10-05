/*
 * (C) Copyright Merative 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

import com.fasterxml.jackson.databind.JsonNode;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Container object describing a data masking operation.
 */
public class DataMaskingObjectModel {

  @Schema(description = "The format of the data to be masked", implementation = String.class)
  private ConfigSchemaTypes schemaType;

  @Schema(description = "Masking configuration")
  private DeidMaskingConfig config;

  @Schema(description = "The data to be masked")
  private JsonNode data;

  /**
   * @return the schemaType
   */
  public ConfigSchemaTypes getSchemaType() {
    return schemaType;
  }
  
  /**
   * @param schemaType the schemaType to set
   */
  public void setSchemaType(ConfigSchemaTypes schemaType) {
    this.schemaType = schemaType;
  }
  
  /**
   * @return the masking configuration
   */
  public DeidMaskingConfig getConfig() {
    return config;
  }
  
  /**
   * @param config the masking configuration to set
   */
  public void setConfig(DeidMaskingConfig config) {
    this.config = config;
  }
  
  /**
   * @return the data
   */
  public JsonNode getData() {
    return data;
  }
  
  /**
   * @param data the data to set
   */
  public void setData(JsonNode data) {
    this.data = data;
  }
}
