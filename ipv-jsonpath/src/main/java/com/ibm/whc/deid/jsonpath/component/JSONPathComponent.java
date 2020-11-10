/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.jsonpath.component;

import com.fasterxml.jackson.databind.JsonNode;
import com.ibm.whc.deid.jsonpath.JSONPathException;
import java.io.Serializable;

public interface JSONPathComponent extends Serializable {
  JsonNode apply(JsonNode obj) throws JSONPathException;
}
