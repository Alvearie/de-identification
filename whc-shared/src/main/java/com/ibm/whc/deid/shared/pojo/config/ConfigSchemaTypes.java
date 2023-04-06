/*
 * © Merative US L.P. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config;

import io.swagger.v3.oas.annotations.media.Schema;

/*
 * Interface for ConfigSchemaTypes - must use.
 */
@Schema(name = "configSchemaTypes", description = "input data format")
public interface ConfigSchemaTypes {
  public String name();
}
