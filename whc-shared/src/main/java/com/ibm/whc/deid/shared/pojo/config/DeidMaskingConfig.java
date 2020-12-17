/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.ibm.whc.deid.shared.pojo.config.json.JsonConfig;

/*
 * The configuration object used for masking requests
 */
@JsonInclude(Include.NON_NULL)
public class DeidMaskingConfig implements Serializable {

  private static final long serialVersionUID = 6474490241730985082L;

  public static final String JSON_CONFIGURATION_PROPERTY_NAME = "json";
  public static final String RULES_CONFIGURATION_PROPERTY_NAME = "rules";
  
  // The rules are configured in the json as an array. So we use List<Rule> in
  // the POJO.
  // But for quicker access, we also store the rule as a map.
  @JsonIgnore
  private Map<String, Rule> rulesMap;
  @JsonProperty(RULES_CONFIGURATION_PROPERTY_NAME)
  private List<Rule> rules;

  @JsonProperty(JSON_CONFIGURATION_PROPERTY_NAME)
  private JsonConfig json;

  @JsonAlias({"certificateID"})
  private String certificateId;

  private boolean defaultNoRuleResolution = true;

  public DeidMaskingConfig() {
    // nothing required here
  }

  @JsonIgnore
  public Map<String, Rule> getRulesMap() {
    return rulesMap;
  }

  public List<Rule> getRules() {
    return rules;
  }

  public void setRules(List<Rule> rules) {
    this.rules = rules;
    
    // Add all the rules to a hashmap for quicker access
    rulesMap = new HashMap<>(rules == null ? 10 : rules.size() * 2);
    if (rules != null) {
      for (Rule rule : rules) {        
        if (rule != null) {
          rulesMap.put(rule.getName(), rule);
        }
      }
    }
  }

  public JsonConfig getJson() {
    return json;
  }

  public void setJson(JsonConfig json) {
    this.json = json;
  }

  public boolean isDefaultNoRuleResolution() {
    return defaultNoRuleResolution;
  }

  public void setDefaultNoRuleResolution(boolean defaultNoRuleResolution) {
    this.defaultNoRuleResolution = defaultNoRuleResolution;
  }

  public String getCertificateId() {
    return certificateId;
  }

  public void setCertificateId(String certificateId) {
    this.certificateId = certificateId;
  }
}
