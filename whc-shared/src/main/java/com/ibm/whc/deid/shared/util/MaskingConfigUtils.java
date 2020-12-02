/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.util;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.Rule;
import com.ibm.whc.deid.shared.pojo.config.json.JsonConfig;
import com.ibm.whc.deid.shared.pojo.config.json.JsonMaskingRule;
import com.ibm.whc.deid.shared.pojo.config.masking.ConfigConstant;
import com.ibm.whc.deid.shared.pojo.config.masking.MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType.MaskingProviderCategory;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/*
 * Utility class for masking config functionality
 */
public class MaskingConfigUtils {

  private static final Logger logger = LoggerFactory.getLogger(MaskingConfigUtils.class);

  private static final MaskingConfigUtils _instance = new MaskingConfigUtils();

  public static MaskingConfigUtils getInstance() {
    return _instance;
  }

  protected ObjectMapper getObjectMapper() {
    return ObjectMapperFactory.getObjectMapper();
  }

  /**
   * Get a DeidMaskingConfig for testing
   *
   * @param maskingRules
   * @param maskingProviders
   * @return
   */
  public static DeidMaskingConfig getDeidConfig(String maskingRules, String maskingProviders) {

    DeidMaskingConfig config = new DeidMaskingConfig();

    List<Rule> rules = getFhirRules(maskingProviders);
    config.setRules(rules);

    config.setJson(getDefaultFhirConfig(maskingRules));

    return config;
  }

  /**
   * Handy method to create a {@link Rule} with a single masking provider config
   *
   * @param name
   * @param config
   * @return
   */
  public static Rule createRuleWithOneProvider(String name, MaskingProviderConfig config) {
    List<MaskingProviderConfig> maskingProviders = new ArrayList<>();
    maskingProviders.add(config);
    Rule rule = new Rule(name, maskingProviders);
    return rule;
  }

  /**
   * Get a set of default rules
   *
   * @return
   */
  public static List<Rule> getFhirRules(String maskingProviders) {
    List<Rule> rules = new ArrayList<>();
    ObjectMapper mapper = new ObjectMapper();

    try {
      JsonNode providers = mapper.readTree(maskingProviders);

      Iterator<Entry<String, JsonNode>> jsonFields = providers.fields();
      while (jsonFields.hasNext()) {
        Entry<String, JsonNode> field = jsonFields.next();
        String ruleName = field.getKey();
        String type = null;
        JsonNode defaultMaskingProvider =
            field.getValue().get(ConfigConstant.DEFAULT_MASKING_PROVIDER);
        if (defaultMaskingProvider != null) {
          type = defaultMaskingProvider.asText();
          MaskingProviderConfig config = MaskingProviderConfig
              .getDefaultMaskingProviderConfig(MaskingProviderType.valueOf(type));
          Rule rule = createRuleWithOneProvider(ruleName, config);

          rules.add(rule);
        }
      }
    } catch (IOException e) {
      logger.error(e.getMessage(), e);
    }

    return rules;
  }

  /**
   * Read a resource file as one string.
   *
   * @param resource
   * @return
   * @throws IOException
   */
  public static String readResourceFileAsString(String resource) throws IOException {
    try (InputStream is = MaskingConfigUtils.class.getResourceAsStream(resource)) {
      try (InputStreamReader isr = new InputStreamReader(is);
          BufferedReader reader = new BufferedReader(isr)) {
        return reader.lines().collect(Collectors.joining());
      }
    }
  }

  /**
   * Read an InputStream as one string
   *
   * @param is
   * @return
   * @throws IOException
   */
  public static String readResourceFileAsString(InputStream is) throws IOException {
    try (InputStreamReader isr = new InputStreamReader(is);
        BufferedReader reader = new BufferedReader(isr)) {
      return reader.lines().collect(Collectors.joining());
    }
  }

  /**
   * Get the default fhir masking rules.
   *
   * @return
   */
  public static List<JsonMaskingRule> getDefaultFhirMaskingRules(String maskingRulesJson) {
    List<JsonMaskingRule> maskingRules = new ArrayList<>();
    ObjectMapper mapper = new ObjectMapper();
    try {

      JsonNode providers = mapper.readTree(maskingRulesJson);

      Iterator<Entry<String, JsonNode>> jsonFields = providers.fields();
      while (jsonFields.hasNext()) {
        Entry<String, JsonNode> field = jsonFields.next();
        String jsonPath = field.getKey();
        String rule = field.getValue().asText();

        JsonMaskingRule maskingRule = new JsonMaskingRule(jsonPath, rule);
        maskingRules.add(maskingRule);
      }
    } catch (IOException e) {
      logger.error(e.getMessage(), e);
    }
    return maskingRules;
  }

  /**
   * Get MessageTypes for FHIR. Used for testing
   *
   * @return
   */
  public static List<String> getFhirTestMessageTypes() {
    String[] messageTypes = {"Questionnaire", "Device", "DeviceMetric", "DeviceComponent",
        "Patient", "Practitioner", "Location", "Organization", "Observation", "Medication",
        "MedicationOrder", "MedicationAdministration", "Contract", "QuestionnaireResponse",
        "BodySite", "Group", "CarePlan", "AuditEvent",};

    return Arrays.asList(messageTypes);
  }

  public static JsonConfig getDefaultFhirConfig(String maskingRules) {
    JsonConfig jsonConfig = new JsonConfig();

    jsonConfig.setMessageTypes(getFhirTestMessageTypes());

    jsonConfig.setSchemaType(ConfigSchemaType.FHIR);

    // For most of existing test cases, we use resourceType
    jsonConfig.setMessageTypeKey("resourceType");

    jsonConfig.setMaskingRules(getDefaultFhirMaskingRules(maskingRules));

    return jsonConfig;
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
      deidMaskingConfig = getObjectMapper().readValue(configuration, DeidMaskingConfig.class);
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
              "invalid masking configuration: the value of the `" + Rule.NAME_PROPERTY_NAME
                  + "` property in the rule at offset " + offset + " in `"
                  + DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME
                  + "` has already been used by another rule",
              DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "." + Rule.NAME_PROPERTY_NAME);
        }

        List<MaskingProviderConfig> providers = rule.getMaskingProviders();
        if (providers == null || providers.isEmpty()) {
          throw new InvalidMaskingConfigurationException(
              "invalid masking configuration: the `" + Rule.PROVIDERS_PROPERTY_NAME
                  + "` property is missing from the rule at offset " + offset + " in `"
                  + DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "`",
              DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "."
                  + Rule.PROVIDERS_PROPERTY_NAME);
        }

        if (providers.size() > 2) {
          throw new InvalidMaskingConfigurationException(
              "invalid masking configuration: too many entries in `" + Rule.PROVIDERS_PROPERTY_NAME
                  + "` for the rule at offset " + offset + " in `"
                  + DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME
                  + "` - the maximum allowed is 2",
              DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "."
                  + Rule.PROVIDERS_PROPERTY_NAME);
        }

        int providerOffset = 0;
        for (MaskingProviderConfig provider : providers) {
          if (provider == null) {
            throw new InvalidMaskingConfigurationException(
                "invalid masking configuration: the masking provider at offset " + providerOffset
                    + " in `" + Rule.PROVIDERS_PROPERTY_NAME + "` for the rule at offset " + offset
                    + " in `" + DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "` is null",
                DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "."
                    + Rule.PROVIDERS_PROPERTY_NAME);
          }

          // the `type` property in each masking provider could not have been deserialized without
          // being valid

          try {
            provider.validate();
          } catch (InvalidMaskingConfigurationException e) {
            String location = DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "."
                + Rule.PROVIDERS_PROPERTY_NAME;
            if (e.getLocation() != null) {
              location += ("." + e.getLocation());
            }
            throw new InvalidMaskingConfigurationException(
                "invalid masking configuration: the masking provider at offset " + providerOffset
                    + " in `" + Rule.PROVIDERS_PROPERTY_NAME + "` for the rule at offset " + offset
                    + " in `" + DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME
                    + "` is not valid: " + e.getMessage(),
                e, location);
          }

          providerOffset++;
        }

        if (providers.size() == 2) {
          if (providers.get(0).getType().getCategory() == MaskingProviderCategory.CategoryII) {
            throw new InvalidMaskingConfigurationException(
                "invalid masking configuration: the rule at offset " + offset + " in `"
                    + DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME
                    + "` contains multiple masking providers, but the first masking provider is not a Category I provider",
                DeidMaskingConfig.RULES_CONFIGURATION_PROPERTY_NAME + "."
                    + Rule.PROVIDERS_PROPERTY_NAME);
          }
          if (providers.get(1).getType().getCategory() == MaskingProviderCategory.CategoryI) {
            throw new InvalidMaskingConfigurationException(
                "invalid masking configuration: the rule at offset " + offset + " in `"
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
