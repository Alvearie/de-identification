/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.jsonpath.component;

import com.fasterxml.jackson.databind.JsonNode;
import com.ibm.whc.deid.jsonpath.JSONPathException;

public class FieldComponent implements JSONPathComponent {

  /**
   * 
   */
  private static final long serialVersionUID = -3275428427357251216L;
  protected String fieldName;

  protected FieldComponent() {}

  public FieldComponent(String fieldName) {
    this.fieldName = fieldName;
  }

  @Override
  public JsonNode apply(JsonNode obj) throws JSONPathException {
    if (obj.isObject() && obj.has(fieldName))
      return obj.get(fieldName);

    throw new JSONPathException("Incompatible object: missing " + fieldName + " " + obj.toString());
  }
}
