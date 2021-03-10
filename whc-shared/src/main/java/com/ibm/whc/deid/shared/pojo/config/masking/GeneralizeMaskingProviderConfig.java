/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/*
 * Configuration for provider that replaces one or more specified original values with a specified
 * general category term to which these values belong.
 */
@JsonInclude(Include.NON_NULL)
public class GeneralizeMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 1395588026091172621L;

  private static final String JSON_TARGETVALUE_TAG = "targetValue";
  private static final String JSON_SOURCE_VALUE_IN_TAG = "sourceValueIn";
  private static final String JSON_SOURCE_VALUE_NOTIN_TAG = "sourceValueNotIn";

  private String maskRuleSet = null;

  public GeneralizeMaskingProviderConfig() {
    type = MaskingProviderType.GENERALIZE;
  }

  public String getMaskRuleSet() {
    return maskRuleSet;
  }

  public void setMaskRuleSet(String maskRuleSet) {
    this.maskRuleSet = maskRuleSet;
  }

  public List<GeneralizeRule> parseMaskRuleSet(String ruleSetStr)
      throws InvalidMaskingConfigurationException {
    List<GeneralizeRule> rulesetList = new ArrayList<>();
    if (ruleSetStr != null) {
      try {
        ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
        JsonNode ruleSetNode = mapper.readTree(ruleSetStr);
        if (ruleSetNode != null && !ruleSetNode.isMissingNode() && !ruleSetNode.isNull()) {
          if (!ruleSetNode.isArray()) {
            throw new InvalidMaskingConfigurationException(
                "`maskRuleSet` value must be a valid json array");
          }
          int index = 0;
          for (JsonNode ruleNode : ruleSetNode) {
            String targetValue = null;
            if (ruleNode.has(JSON_TARGETVALUE_TAG)) {
              JsonNode targetValueNode = ruleNode.path(JSON_TARGETVALUE_TAG);
              if (targetValueNode.isTextual()) {
                targetValue = targetValueNode.asText();
              }
            }
            if (targetValue == null) {
              throw new InvalidMaskingConfigurationException(
                  "`" + JSON_TARGETVALUE_TAG + "` is null for value set " + index);
            }

            JsonNode sourceValueInNode = null;
            JsonNode sourceValueNotInNode = null;
            if (ruleNode.has(JSON_SOURCE_VALUE_IN_TAG)) {
              sourceValueInNode = ruleNode.path(JSON_SOURCE_VALUE_IN_TAG);
              if (sourceValueInNode.isNull()) {
                sourceValueInNode = null;
              }
            }
            if (ruleNode.has(JSON_SOURCE_VALUE_NOTIN_TAG)) {
              sourceValueNotInNode = ruleNode.path(JSON_SOURCE_VALUE_NOTIN_TAG);
              if (sourceValueNotInNode.isNull()) {
                sourceValueNotInNode = null;
              }
            }
            if (sourceValueInNode == null && sourceValueNotInNode == null) {
              throw new InvalidMaskingConfigurationException(
                  "one of `" + JSON_SOURCE_VALUE_IN_TAG + "` or `" + JSON_SOURCE_VALUE_NOTIN_TAG
                      + "` must be specified in value set " + index);
            }
            if (sourceValueInNode != null && sourceValueNotInNode != null) {
              throw new InvalidMaskingConfigurationException(
                  "only one of `" + JSON_SOURCE_VALUE_IN_TAG + "` and `"
                      + JSON_SOURCE_VALUE_NOTIN_TAG + "` can be specified in value set " + index);
            }
            if (sourceValueInNode != null && !sourceValueInNode.isArray()) {
              throw new InvalidMaskingConfigurationException(
                  "`" + JSON_SOURCE_VALUE_IN_TAG + "` must be a json array in value set " + index);
            }
            if (sourceValueNotInNode != null && !sourceValueNotInNode.isArray()) {
              throw new InvalidMaskingConfigurationException("`" + JSON_SOURCE_VALUE_NOTIN_TAG
                  + "` must be a json array in value set " + index);
            }

            GeneralizeRule rule = new GeneralizeRule(targetValue);

            if (sourceValueInNode != null) {
              rule.setLogicalNegation(false);
              for (JsonNode sourceValueIn : sourceValueInNode) {
                if (sourceValueIn.isNull()) {
                  throw new InvalidMaskingConfigurationException(
                      "null object specified in list of values in value set " + index
                          + " - only textual data is allowed");
                }
                String value = sourceValueIn.asText();
                rule.getValueSet().add(value);
              }
            } else {
              rule.setLogicalNegation(true);
              for (JsonNode sourceValueNotIn : sourceValueNotInNode) {
                if (sourceValueNotIn.isNull()) {
                  throw new InvalidMaskingConfigurationException(
                      "null object specified in list of values in value set " + index
                          + " - only textual data is allowed");
                }
                String value = sourceValueNotIn.asText();
                rule.getValueSet().add(value);
              }
            }
            if (rule.getValueSet().isEmpty()) {
              throw new InvalidMaskingConfigurationException(
                  "at least one value must be supplied in list of values in value set " + index);
            }

            rulesetList.add(rule);
            index++;
          }
        }
      } catch (IOException e) {
        throw new InvalidMaskingConfigurationException(
            "`maskRuleSet` is not valid json - " + e.getMessage());
      }
    }
    return rulesetList;
  }

  @Override
  public void validate(DeidMaskingConfig maskingConfig)
      throws InvalidMaskingConfigurationException {
    super.validate(maskingConfig);
    parseMaskRuleSet(getMaskRuleSet());
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((maskRuleSet == null) ? 0 : maskRuleSet.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (!super.equals(obj))
      return false;
    if (getClass() != obj.getClass())
      return false;
    GeneralizeMaskingProviderConfig other = (GeneralizeMaskingProviderConfig) obj;
    if (maskRuleSet == null) {
      if (other.maskRuleSet != null)
        return false;
    } else if (!maskRuleSet.equals(other.maskRuleSet))
      return false;
    return true;
  }


  public static class GeneralizeRule implements Serializable {

    private static final long serialVersionUID = -891577319390297701L;

    private boolean logicalNegation;
    private String category;
    private Set<String> valueSet;

    GeneralizeRule(String category) {
      this.category = category;
      logicalNegation = false; // default to false
      valueSet = new HashSet<>();
    }

    public void setLogicalNegation(boolean logicalNegation) {
      this.logicalNegation = logicalNegation;
    }

    public boolean getLogicalNegation() {
      return logicalNegation;
    }

    public String getCategory() {
      return category;
    }

    public Set<String> getValueSet() {
      return valueSet;
    }
  }
}
