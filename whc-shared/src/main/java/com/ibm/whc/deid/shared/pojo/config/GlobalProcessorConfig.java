/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config;

import java.io.Serializable;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.exception.InvalidInputException;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The configuration of the global processor, which can manipulate the structure
 * and content of the input documents before rule-level data masking occurs.
 */
@JsonInclude(Include.NON_NULL)
public class GlobalProcessorConfig implements Serializable {

  private static final long serialVersionUID = 1L;

  public static final String RULE_SET_PROPERTY_NAME = "ruleSet";
  
  public static final String DEFAULT_RULE_SET = "default";

  @JsonProperty(RULE_SET_PROPERTY_NAME)
  private String ruleSet;

  public GlobalProcessorConfig() {
    // nothing required here
  }

  public String getRuleSet() {
    return ruleSet;
  }

  public void setRuleSet(String ruleSet) {
    this.ruleSet = ruleSet;
  }

  /**
   * Determines whether this object contains allowed, supported values.
   * 
   * @throws InvalidInputException if any part of the content is not valid.
   */
  public void validate() throws InvalidInputException {
    // only allowed non-empty value for ruleSet is the default ruleSet
    if (ruleSet != null && !ruleSet.isEmpty()
        && !ruleSet.equalsIgnoreCase(DEFAULT_RULE_SET)) {
      throw new InvalidInputException("illegal value in global configuration");
    }
  }
}
