/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.util;

import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.Rule;
import com.ibm.whc.deid.shared.pojo.config.json.JsonConfig;
import com.ibm.whc.deid.shared.pojo.config.json.JsonMaskingRule;
import com.ibm.whc.deid.shared.pojo.config.masking.MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType.MaskingProviderCategory;

/*
 * Class containing masking configuration utilities
 */
public class MaskingConfigUtils {

  private static final MaskingConfigUtils _instance = new MaskingConfigUtils();

  public static MaskingConfigUtils getInstance() {
    return _instance;
  }

  /**
   * Validates a masking configuration.
   *
   * @param configuration the masking configuration in serialized JSON (String) form
   * 
   * @return the configuration in object form
   * 
   * @throws InvalidMaskingConfigurationException if the given configuration string is not valid
   */
  public static DeidMaskingConfig validateConfig(String configuration)
      throws InvalidMaskingConfigurationException {
    return getInstance().validateConfigMethod(configuration);
  }

  /**
   * Validates a masking configuration.
   *
   * @param configuration the masking configuration in serialized JSON (String) form
   * 
   * @return the configuration in object form
   * 
   * @throws InvalidMaskingConfigurationException if the given configuration string is not valid
   */
  public DeidMaskingConfig validateConfigMethod(String configuration)
      throws InvalidMaskingConfigurationException {
    DeidMaskingConfig deidMaskingConfig = null;

    if (configuration == null) {
      throw new InvalidMaskingConfigurationException("no configuration data");
    }

    try {
      deidMaskingConfig =
          ObjectMapperFactory.getObjectMapper().readValue(configuration, DeidMaskingConfig.class);
    } catch (IOException e) {
      throw new InvalidMaskingConfigurationException("invalid configuration: " + e.getMessage(), e);
    }

    validateRules(deidMaskingConfig);
    validateJsonConfig(deidMaskingConfig, false);

    return deidMaskingConfig;
  }

  /**
   * Validates the content of the "json" property in a masking configuration.
   *
   * @param deidMaskingConfig the masking configuration being validated
   * 
   * @param allowsNullRuleInRuleAssignment <i>True</i> if rule assignments should be accepted if
   *        they do not actually specify a rule and <i>False</i> otherwise. Any rule assignments
   *        accepted under the value <i>True</i> must be removed or modified before a masking
   *        operation uses this configuration.
   * 
   * @throws InvalidMaskingConfigurationException if the masking configuration is not valid.
   */
  protected void validateJsonConfig(DeidMaskingConfig deidMaskingConfig,
      boolean allowsNullRuleInRuleAssignment) throws InvalidMaskingConfigurationException {

    JsonConfig jsonConfig = deidMaskingConfig.getJson();
    if (jsonConfig == null) {
      throw new InvalidMaskingConfigurationException(
          "invalid masking configuration: the value of the `"
              + DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "` property is missing",
          DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME);
    }

    if (jsonConfig.getSchemaType() == null) {
      throw new InvalidMaskingConfigurationException(
          "invalid masking configuration: the value of the `"
              + DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
              + JsonConfig.SCHEMA_TYPE_PROPERTY_NAME + "` property is missing",
          DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
              + JsonConfig.SCHEMA_TYPE_PROPERTY_NAME);
    }

    String messageTypeKey = jsonConfig.getMessageTypeKey();
    if (messageTypeKey != null && !messageTypeKey.trim().isEmpty()) {
      // when messageTypeKey is provided, messageTypes must be provided
      List<String> messageTypes = jsonConfig.getMessageTypes();
      if (messageTypes == null || messageTypes.isEmpty()) {
        throw new InvalidMaskingConfigurationException(
            "invalid masking configuration: `" + DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME
                + "." + JsonConfig.MESSAGE_TYPES_PROPERTY_NAME + "` must be provided when `"
                + DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                + JsonConfig.MESSAGE_TYPE_KEY_PROPERTY_NAME + "` is provided",
            DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                + JsonConfig.MESSAGE_TYPES_PROPERTY_NAME);
      }
      int offset = 0;
      for (String messageType : messageTypes) {
        if (messageType == null || messageType.trim().isEmpty()) {
          throw new InvalidMaskingConfigurationException(
              "invalid masking configuration: value at offset " + offset + " in `"
                  + DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                  + JsonConfig.MESSAGE_TYPES_PROPERTY_NAME + "` is missing",
              DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                  + JsonConfig.MESSAGE_TYPES_PROPERTY_NAME);
        }
        offset++;
      }
    }

    List<JsonMaskingRule> maskingRules = jsonConfig.getMaskingRules();
    if (maskingRules != null && !maskingRules.isEmpty()) {
      Map<String, Rule> rulesMap = deidMaskingConfig.getRulesMap();
      int offset = 0;
      int mismatchCount = 0;
      String firstMismatch = null;

      for (JsonMaskingRule ruleAssignment : maskingRules) {
        if (ruleAssignment == null) {
          throw new InvalidMaskingConfigurationException(
              "invalid masking configuration: `" + JsonMaskingRule.RULE_PROPERTY_NAME
                  + "` property is missing from the rule assignment at offset " + offset + " in `"
                  + DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                  + JsonConfig.RULES_PROPERTY_NAME + "`",
              DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                  + JsonConfig.RULES_PROPERTY_NAME + "." + JsonMaskingRule.RULE_PROPERTY_NAME);
        }

        String path = ruleAssignment.getJsonPath();
        if (path == null || !path.startsWith("/")) {
          throw new InvalidMaskingConfigurationException(
              "invalid masking configuration: `" + JsonMaskingRule.PATH_PROPERTY_NAME
                  + "` property in the rule assignment at offset " + offset + " in `"
                  + DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                  + JsonConfig.RULES_PROPERTY_NAME + "` must start with `/`",
              DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                  + JsonConfig.RULES_PROPERTY_NAME + "." + JsonMaskingRule.PATH_PROPERTY_NAME);
        }

        String ruleName = ruleAssignment.getRule();
        if (ruleName == null) {
          if (!allowsNullRuleInRuleAssignment) {
            throw new InvalidMaskingConfigurationException(
                "invalid masking configuration: `" + JsonMaskingRule.RULE_PROPERTY_NAME
                    + "` property is missing from the rule assignment at offset " + offset + " in `"
                    + DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                    + JsonConfig.RULES_PROPERTY_NAME + "`",
                DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                    + JsonConfig.RULES_PROPERTY_NAME + "." + JsonMaskingRule.RULE_PROPERTY_NAME);
          }
        } else {
          if (!rulesMap.containsKey(ruleName)) {
            mismatchCount++;
            if (firstMismatch == null) {
              firstMismatch = ruleName;
            }
          }
        }

        offset++;
      }

      if (firstMismatch != null) {
        throw new InvalidMaskingConfigurationException(
            "The JSON masking rule does not refer to a valid rule: " + firstMismatch
                + ". There are " + mismatchCount + " invalid rules.");
      }
    }
  }

  /**
   * Validates the content of the "rules" property in a masking configuration.
   *
   * @param deidMaskingConfig the masking configuration being validated
   * 
   * @throws InvalidMaskingConfigurationException if the masking configuration is not valid.
   */
  protected void validateRules(DeidMaskingConfig deidMaskingConfig)
      throws InvalidMaskingConfigurationException {

    List<Rule> rules = deidMaskingConfig.getRules();
    // it is valid for the list of rules to be missing or empty
    if (rules != null && !rules.isEmpty()) {
      HashSet<String> ruleNames = new HashSet<>(rules.size() * 2);
      int offset = 0;
      for (Rule rule : rules) {
        if (rule == null) {
          throw new InvalidMaskingConfigurationException(
              "invalid masking configuration: the rule at offset " + offset + " in `"
                  + DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "` is null",
              DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME);
        }

        String ruleName = rule.getName();
        if (ruleName == null || ruleName.trim().isEmpty()) {
          throw new InvalidMaskingConfigurationException(
              "invalid masking configuration: the `" + Rule.NAME_PROPERTY_NAME
                  + "` property is missing from the rule at offset " + offset + " in `"
                  + DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "`",
              DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "." + Rule.NAME_PROPERTY_NAME);
        }

        if (!ruleNames.add(ruleName)) {
          throw new InvalidMaskingConfigurationException(
              "invalid masking configuration: the value `" + ruleName + "` is used for the `"
                  + Rule.NAME_PROPERTY_NAME + "` property on multiple rules in the `"
                  + DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME
                  + "` list - rule names must be unique",
              DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "." + Rule.NAME_PROPERTY_NAME);
        }

        List<MaskingProviderConfig> providers = rule.getMaskingProviders();
        if (providers == null || providers.isEmpty()) {
          throw new InvalidMaskingConfigurationException(
              "invalid masking configuration: the `" + Rule.PROVIDERS_PROPERTY_NAME
                  + "` property is missing from the rule with `" + Rule.NAME_PROPERTY_NAME
                  + "` value `" + ruleName + "` in `"
                  + DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "`",
              DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "."
                  + Rule.PROVIDERS_PROPERTY_NAME);
        }

        if (providers.size() > 2) {
          throw new InvalidMaskingConfigurationException(
              "invalid masking configuration: too many entries in `" + Rule.PROVIDERS_PROPERTY_NAME
                  + "` for the rule with `" + Rule.NAME_PROPERTY_NAME + "` value `" + ruleName
                  + "` in `" + DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME
                  + "` - the maximum allowed is 2",
              DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "."
                  + Rule.PROVIDERS_PROPERTY_NAME);
        }

        int providerOffset = 0;
        for (MaskingProviderConfig provider : providers) {
          if (provider == null) {
            StringBuilder buffer = new StringBuilder(200);
            buffer.append(DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME);
            buffer.append('.');
            buffer.append(Rule.PROVIDERS_PROPERTY_NAME);
            String location = buffer.toString();
            buffer.setLength(0);
            buffer.append("invalid masking configuration: the ");
            buffer.append(providerOffset == 0 ? "first" : "second");
            buffer.append(" masking provider in `");
            buffer.append(Rule.PROVIDERS_PROPERTY_NAME);
            buffer.append("` for the rule with `");
            buffer.append(Rule.NAME_PROPERTY_NAME);
            buffer.append("` value `");
            buffer.append(ruleName);
            buffer.append("` in `");
            buffer.append(DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME);
            buffer.append("` is null");
            String message = buffer.toString();
            throw new InvalidMaskingConfigurationException(message, location);
          }

          // the `type` property in each masking provider could not have been deserialized without
          // being valid

          try {
            provider.validate();
          } catch (InvalidMaskingConfigurationException e) {
            StringBuilder buffer = new StringBuilder(200);
            buffer.append(DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME);
            buffer.append('.');
            buffer.append(Rule.PROVIDERS_PROPERTY_NAME);
            if (e.getLocation() != null) {
              buffer.append('.').append(e.getLocation());
            }
            String location = buffer.toString();
            buffer.setLength(0);
            buffer.append("invalid masking configuration: the ");
            buffer.append(providerOffset == 0 ? "first" : "second");
            buffer.append(" masking provider in `");
            buffer.append(Rule.PROVIDERS_PROPERTY_NAME);
            buffer.append("` for the rule with `");
            buffer.append(Rule.NAME_PROPERTY_NAME);
            buffer.append("` value `");
            buffer.append(ruleName);
            buffer.append("` in `");
            buffer.append(DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME);
            buffer.append("` is not valid: ");
            buffer.append(e.getMessage());
            String message = buffer.toString();
            throw new InvalidMaskingConfigurationException(message, e, location);
          }

          providerOffset++;
        }

        if (providers.size() == 2) {
          if (providers.get(0).getType().getCategory() == MaskingProviderCategory.CategoryII) {
            throw new InvalidMaskingConfigurationException(
                "invalid masking configuration: the rule with `" + Rule.NAME_PROPERTY_NAME
                    + "` value `" + ruleName + "` in `"
                    + DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME
                    + "` contains multiple masking providers, but the first masking provider is not a Category I provider",
                DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "."
                    + Rule.PROVIDERS_PROPERTY_NAME);
          }
          if (providers.get(1).getType().getCategory() == MaskingProviderCategory.CategoryI) {
            throw new InvalidMaskingConfigurationException(
                "invalid masking configuration: the rule with `" + Rule.NAME_PROPERTY_NAME
                    + "` value `" + ruleName + "` in `"
                    + DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME
                    + "` contains multiple masking providers, but the second masking provider is not a Category II provider",
                DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "."
                    + Rule.PROVIDERS_PROPERTY_NAME);
          }
        }

        offset++;
      }
    }
  }
}
