/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
    int capacity = rules == null ? 2 : Math.round(rules.size() / 0.75f) + 1;
    Map<String, Rule> rulesMap = new HashMap<>(capacity);
    if (rules != null) {
      for (Rule rule : rules) {        
        if (rule != null) {
          rulesMap.put(rule.getName(), rule);
        }
      }
    }
    return rulesMap;
  }

  public List<Rule> getRules() {
    return rules;
  }

  public void setRules(List<Rule> rules) {
    this.rules = rules;
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
