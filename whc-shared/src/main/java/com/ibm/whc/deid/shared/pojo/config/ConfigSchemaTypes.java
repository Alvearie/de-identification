/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config;

import io.swagger.v3.oas.annotations.media.Schema;

/*
 * Interface for ConfigSchemaTypes - must use.
 */
@Schema(name = "configSchemaTypes", description = "Type of config schema for the data")
public interface ConfigSchemaTypes {
  public String name();
}
