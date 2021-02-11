/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

public class ConfigGenerator {

  private static final Logger logger = LoggerFactory.getLogger(ConfigGenerator.class);

  protected DeidMaskingConfig getNewMaskingConfig() {
    return new DeidMaskingConfig();
  }

  /**
   * Get a default masking config for testing FHIR
   *
   * @return
   */
  public DeidMaskingConfig getTestDeidConfig() {
    DeidMaskingConfig config = getNewMaskingConfig();
    try {
      String maskingProviders =
          ConfigGenerator.readResourceFileAsString("/template/fhir_masking_providers.json");
      String maskingRules =
          ConfigGenerator.readResourceFileAsString("/template/fhir_masking_rules.json");

      List<Rule> rules = ConfigGenerator.getFhirRules(maskingProviders);
      config.setRules(rules);

      config.setJson(ConfigGenerator.getDefaultFhirConfig(maskingRules));

    } catch (IOException e) {
      logger.error(e.getMessage(), e);
    }
    return config;
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
   * Get a set of default rules
   *
   * @return
   */
  public static List<Rule> getFhirRules(String maskingProviders) {
    List<Rule> rules = new ArrayList<>();
    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();

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

  /**
   * Get the default fhir masking rules.
   *
   * @return
   */
  public static List<JsonMaskingRule> getDefaultFhirMaskingRules(String maskingRulesJson) {
    List<JsonMaskingRule> maskingRules = new ArrayList<>();
    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
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
}
