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
import com.ibm.whc.deid.shared.pojo.config.json.JsonConfig;

/*
 * The configuration object used for masking requests
 */
@JsonInclude(Include.NON_NULL)
public class DeidMaskingConfig implements Serializable {

  /** */
  private static final long serialVersionUID = 6474490241730985082L;

  // The rules are configured in the json as an array. So we use List<Rule> in
  // the POJO.
  // But for quicker access, we also store the rule as a hashmap.
  Map<String, Rule> rulesMap;
  List<Rule> rules;

  JsonConfig json;

  @JsonAlias({"certificateID"})
  String certificateId;

  public DeidMaskingConfig() {
    json = new JsonConfig();
  }

  @JsonIgnore
  public Map<String, Rule> getRulesMap() {
    return rulesMap;
  }

  @JsonIgnore
  public void setRulesMap(Map<String, Rule> rulesMap) {
    this.rulesMap = rulesMap;
  }

  public List<Rule> getRules() {
    return rules;
  }

  public void setRules(List<Rule> rules) {
    this.rules = rules;

    // Add all the rules to a hashmap for quicker access
    rulesMap = new HashMap<>();
    rules.forEach(rule -> {
      rulesMap.put(rule.getName(), rule);
    });
  }

  public JsonConfig getJson() {
    return json;
  }

  public void setJson(JsonConfig json) {
    this.json = json;
  }

  boolean defaultNoRuleResolution;

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

  public Map<String, String> getStringValueWithPrefixMatch(String prefix) {
    Map<String, String> values = new ConcurrentHashMap<>();

    json.getMaskingRules().forEach(rule -> {
      if (rule.getJsonPath().startsWith(prefix)) {
        values.put(rule.getJsonPath(), rule.getRule());
      }
    });

    return values;
  }
}
