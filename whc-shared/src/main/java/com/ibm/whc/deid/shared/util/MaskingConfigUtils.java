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
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

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
      InvalidMaskingConfigurationException e = new InvalidMaskingConfigurationException(
          Messages.getMessage(LogCodes.WPH8000E,
              DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME),
          DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME);
      e.setMessageKey(LogCodes.WPH8000E);
      throw e;
    }

    if (jsonConfig.getSchemaType() == null) {
      String property = DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
          + JsonConfig.SCHEMA_TYPE_PROPERTY_NAME;
      InvalidMaskingConfigurationException e = new InvalidMaskingConfigurationException(
          Messages.getMessage(LogCodes.WPH8000E, property), property);
      e.setMessageKey(LogCodes.WPH8000E);
      throw e;
    }

    String messageTypeKey = jsonConfig.getMessageTypeKey();
    if (messageTypeKey != null && !messageTypeKey.trim().isEmpty()) {
      // when messageTypeKey is provided, messageTypes must be provided
      List<String> messageTypes = jsonConfig.getMessageTypes();
      if (messageTypes == null || messageTypes.isEmpty()) {
        String property = DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
            + JsonConfig.MESSAGE_TYPES_PROPERTY_NAME;
        InvalidMaskingConfigurationException e =
            new InvalidMaskingConfigurationException(Messages.getMessage(LogCodes.WPH8001E,
                property, DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                    + JsonConfig.MESSAGE_TYPE_KEY_PROPERTY_NAME),
                property);
        e.setMessageKey(LogCodes.WPH8001E);
        throw e;
      }
      int offset = 0;
      for (String messageType : messageTypes) {
        if (messageType == null || messageType.trim().isEmpty()) {
          String property = DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
              + JsonConfig.MESSAGE_TYPES_PROPERTY_NAME;
          InvalidMaskingConfigurationException e = new InvalidMaskingConfigurationException(
              Messages.getMessage(LogCodes.WPH8002E, String.valueOf(offset), property), property);
          e.setMessageKey(LogCodes.WPH8002E);
          throw e;
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
          InvalidMaskingConfigurationException e = new InvalidMaskingConfigurationException(
              Messages.getMessage(LogCodes.WPH8003E, JsonMaskingRule.RULE_PROPERTY_NAME,
                  String.valueOf(offset),
                  DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                      + JsonConfig.RULES_PROPERTY_NAME),
              DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                  + JsonConfig.RULES_PROPERTY_NAME + "." + JsonMaskingRule.RULE_PROPERTY_NAME);
          e.setMessageKey(LogCodes.WPH8003E);
          throw e;
        }

        String path = ruleAssignment.getJsonPath();
        if (path == null || !path.startsWith("/")) {
          InvalidMaskingConfigurationException e = new InvalidMaskingConfigurationException(
              Messages.getMessage(LogCodes.WPH8004E, JsonMaskingRule.PATH_PROPERTY_NAME,
                  String.valueOf(offset),
                  DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                      + JsonConfig.RULES_PROPERTY_NAME),
              DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                  + JsonConfig.RULES_PROPERTY_NAME + "." + JsonMaskingRule.PATH_PROPERTY_NAME);
          e.setMessageKey(LogCodes.WPH8004E);
          throw e;
        }

        String ruleName = ruleAssignment.getRule();
        if (ruleName == null) {
          if (!allowsNullRuleInRuleAssignment) {
            InvalidMaskingConfigurationException e = new InvalidMaskingConfigurationException(
                Messages.getMessage(LogCodes.WPH8003E, JsonMaskingRule.RULE_PROPERTY_NAME,
                    String.valueOf(offset),
                    DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                        + JsonConfig.RULES_PROPERTY_NAME),
                DeidMaskingConfig.JSON_CONFIGURATION_PROPERTY_NAME + "."
                    + JsonConfig.RULES_PROPERTY_NAME + "." + JsonMaskingRule.RULE_PROPERTY_NAME);
            e.setMessageKey(LogCodes.WPH8003E);
            throw e;
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
        InvalidMaskingConfigurationException e =
            new InvalidMaskingConfigurationException(Messages.getMessage(LogCodes.WPH8005E,
                JsonMaskingRule.RULE_PROPERTY_NAME, firstMismatch, String.valueOf(mismatchCount)));
        e.setMessageKey(LogCodes.WPH8005E);
        throw e;
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
          InvalidMaskingConfigurationException e = new InvalidMaskingConfigurationException(
              Messages.getMessage(LogCodes.WPH8006E, String.valueOf(offset),
                  DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME),
              DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME);
          e.setMessageKey(LogCodes.WPH8006E);
          throw e;
        }

        String ruleName = rule.getName();
        if (ruleName == null || ruleName.trim().isEmpty()) {
          InvalidMaskingConfigurationException e = new InvalidMaskingConfigurationException(
              Messages.getMessage(LogCodes.WPH8007E, Rule.NAME_PROPERTY_NAME,
                  String.valueOf(offset), DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME),
              DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "." + Rule.NAME_PROPERTY_NAME);
          e.setMessageKey(LogCodes.WPH8007E);
          throw e;
        }

        if (!ruleNames.add(ruleName)) {
          InvalidMaskingConfigurationException e = new InvalidMaskingConfigurationException(
              Messages.getMessage(LogCodes.WPH8008E, ruleName, Rule.NAME_PROPERTY_NAME,
                  DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME),
              DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "." + Rule.NAME_PROPERTY_NAME);
          e.setMessageKey(LogCodes.WPH8008E);
          throw e;
        }

        List<MaskingProviderConfig> providers = rule.getMaskingProviders();
        if (providers == null || providers.isEmpty()) {
          InvalidMaskingConfigurationException e = new InvalidMaskingConfigurationException(
              Messages.getMessage(LogCodes.WPH8009E, Rule.PROVIDERS_PROPERTY_NAME,
                  Rule.NAME_PROPERTY_NAME, ruleName,
                  DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME),
              DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "."
                  + Rule.PROVIDERS_PROPERTY_NAME);
          e.setMessageKey(LogCodes.WPH8009E);
          throw e;
        }

        if (providers.size() > 2) {
          InvalidMaskingConfigurationException e = new InvalidMaskingConfigurationException(
              Messages.getMessage(LogCodes.WPH8010E, Rule.PROVIDERS_PROPERTY_NAME,
                  Rule.NAME_PROPERTY_NAME, ruleName,
                  DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME),
              DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "."
                  + Rule.PROVIDERS_PROPERTY_NAME);
          e.setMessageKey(LogCodes.WPH8010E);
          throw e;
        }

        int providerOffset = 0;
        for (MaskingProviderConfig provider : providers) {
          if (provider == null) {
            InvalidMaskingConfigurationException e = new InvalidMaskingConfigurationException(
                Messages.getMessage(LogCodes.WPH8011E, providerOffset == 0 ? "first" : "second",
                    Rule.PROVIDERS_PROPERTY_NAME, Rule.NAME_PROPERTY_NAME, ruleName,
                    DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME),
                DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "."
                    + Rule.PROVIDERS_PROPERTY_NAME);
            e.setMessageKey(LogCodes.WPH8011E);
            throw e;
          }

          // the `type` property in each masking provider could not have been deserialized without
          // being valid

          try {
            provider.validate();
          } catch (InvalidMaskingConfigurationException e) {
            InvalidMaskingConfigurationException e2 = new InvalidMaskingConfigurationException(
                Messages.getMessage(LogCodes.WPH8012E, providerOffset == 0 ? "first" : "second",
                    Rule.PROVIDERS_PROPERTY_NAME, Rule.NAME_PROPERTY_NAME, ruleName,
                    DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME, e.getMessage()),
                DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "."
                    + Rule.PROVIDERS_PROPERTY_NAME);
            e2.setMessageKey(LogCodes.WPH8012E);
            throw e2;
          }

          providerOffset++;
        }

        if (providers.size() == 2) {
          if (providers.get(0).getType().getCategory() == MaskingProviderCategory.CategoryII) {
            InvalidMaskingConfigurationException e = new InvalidMaskingConfigurationException(
                Messages.getMessage(LogCodes.WPH8013E, Rule.NAME_PROPERTY_NAME, ruleName,
                    DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME),
                DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "."
                    + Rule.PROVIDERS_PROPERTY_NAME);
            e.setMessageKey(LogCodes.WPH8013E);
            throw e;
          }
          if (providers.get(1).getType().getCategory() == MaskingProviderCategory.CategoryI) {
            InvalidMaskingConfigurationException e = new InvalidMaskingConfigurationException(
                Messages.getMessage(LogCodes.WPH8014E, Rule.NAME_PROPERTY_NAME, ruleName,
                    DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME),
                DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "."
                    + Rule.PROVIDERS_PROPERTY_NAME);
            e.setMessageKey(LogCodes.WPH8014E);
            throw e;
          }
        }

        offset++;
      }
    }
  }
}
