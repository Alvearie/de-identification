/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.json;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.io.Serializable;

/*
 * JsonMaskingRules used in JsonConfig
 */
public class JsonMaskingRule implements Serializable {

  private static final long serialVersionUID = -1439174548688111947L;

  public static final String PATH_PROPERTY_NAME = "jsonPath";
  public static final String RULE_PROPERTY_NAME = "rule";
  
  @JsonProperty(PATH_PROPERTY_NAME)  
  private String jsonPath;
  @JsonProperty(RULE_PROPERTY_NAME)
  private String rule;

  public JsonMaskingRule() {}

  public JsonMaskingRule(String jsonPath, String rule) {
    this.jsonPath = jsonPath;
    this.rule = rule;
  }

  public String getJsonPath() {
    return jsonPath;
  }

  public void setJsonPath(String jsonPath) {
    this.jsonPath = jsonPath;
  }

  public String getRule() {
    return rule;
  }

  public void setRule(String rule) {
    this.rule = rule;
  }

  @Override
  public String toString() {
    return "JsonMaskingRule [jsonPath=" + jsonPath + ", rule=" + rule + "]";
  }
}
