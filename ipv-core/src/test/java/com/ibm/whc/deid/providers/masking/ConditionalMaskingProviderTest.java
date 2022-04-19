/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.providers.masking.fhir.MaskingActionInputIdentifier;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.ConditionalMaskRuleSet;
import com.ibm.whc.deid.shared.pojo.config.masking.ConditionalMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.HashMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.PseudonymMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.RedactMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition;
import com.ibm.whc.deid.shared.pojo.config.masking.conditional.ConditionOperator;
import com.ibm.whc.deid.shared.pojo.config.masking.conditional.ConditionType;
import com.ibm.whc.deid.shared.util.ConfigGenerator;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class ConditionalMaskingProviderTest {

  private class TestConditionalMaskingProvider extends ConditionalMaskingProvider {

    private static final long serialVersionUID = 1L;

    public final List<Set<String>> valueSets = new ArrayList<>();

    public TestConditionalMaskingProvider(ConditionalMaskingProviderConfig configuration,
        String tenantId, DeidMaskingConfig deidMaskingConfig, String localizationProperty,
        MaskingProviderFactory maskingProviderFactory) {
      super(configuration, tenantId, deidMaskingConfig, localizationProperty,
          maskingProviderFactory);
    }

    @Override
    protected Set<String> getConditionRegularFieldValues(Condition condition, JsonNode root,
        String resourceType) {
      Set<String> vs = super.getConditionRegularFieldValues(condition, root, resourceType);
      valueSets.add(vs);
      return vs;
    }
  }

  private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

  String tenantId = "TEST_TENANT";
  DeidMaskingConfig deidMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();

  /*
   * Test conditional regular field path
   */
  @Test
  public void testConditionalRegularPath() throws Exception {
    ConditionalMaskingProviderConfig conditionalMaskingProviderConfig =
        new ConditionalMaskingProviderConfig();
    PseudonymMaskingProviderConfig pseudonymConfig = new PseudonymMaskingProviderConfig();
    pseudonymConfig.setGenerateViaOptionsMinLength(12);
    pseudonymConfig.setGenerateViaOptionsMaxLength(12);
    com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition condition = new Condition();
    condition.setField("text/status");
    condition.setOperator(ConditionOperator.EQUALS);
    condition.setType(ConditionType.STRING);
    condition.setValue("mask_condition");
    List<ConditionalMaskRuleSet> maskRuleSet = new ArrayList<ConditionalMaskRuleSet>();
    ConditionalMaskRuleSet ruleSet1 = new ConditionalMaskRuleSet();
    ruleSet1.setCondition(condition);
    ruleSet1.setMaskingProvider(pseudonymConfig);
    conditionalMaskingProviderConfig.setMaskRuleSet(maskRuleSet);

    ConditionalMaskRuleSet ruleSet2 = new ConditionalMaskRuleSet();
    ruleSet2.setMaskingProvider(new HashMaskingProviderConfig());
    maskRuleSet.add(ruleSet1);
    maskRuleSet.add(ruleSet2);

    String utLine = "{" + "      \"resourceType\": \"Device\"," + "      \"id\": \"example\","
        + "      \"text\":{\"fhir_comments\":[\"text\"],\"status\":\"mask_condition\"},"
        + "      \"active\": false," + "      \"status\": \"confirmed\","
        + "      \"version\": \"2.6\","
        + "      \"telecom\":[{\"system\":\"phone\", \"value\":\"123456789-987654321\"}],"
        + "      \"extension\":["
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName\", \"valueString\":\"Asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    System.out.println(mapper.writeValueAsString(conditionalMaskingProviderConfig));

    ConditionalMaskingProvider maskingProvider =
        new ConditionalMaskingProvider(conditionalMaskingProviderConfig, tenantId,
            deidMaskingConfig, localizationProperty, new BasicMaskingProviderFactory());

    JsonNode nodeRoot = mapper.readTree(utLine);
    JsonNode nodeParent = nodeRoot.get("text");
    JsonNode inputDataNode = nodeParent.get("status");
    String identifier = inputDataNode.asText();
    MaskingActionInputIdentifier maii = new MaskingActionInputIdentifier(null, inputDataNode,
        nodeParent, "status", "", "", nodeRoot);
    maskingProvider.maskIdentifierBatch(Arrays.asList(maii));
    String maskedValue = maii.getRoot().get("text").get("status").asText();
    // System.out.println("\n=======> testConditionalRegularPath identifier:
    // "
    // + identifier + " maskedValue: " + maskedValue);

    assertTrue(!maskedValue.equals(identifier));
    assertTrue(maskedValue.length() == 12);
  }

  /*
   * Test conditional array query field path
   */
  @Test
  public void testConditionalArrayFieldWithoutQueryIndex() throws Exception {

    ConditionalMaskingProviderConfig conditionalMaskingProviderConfig =
        new ConditionalMaskingProviderConfig();
    PseudonymMaskingProviderConfig pseudonymConfig = new PseudonymMaskingProviderConfig();
    pseudonymConfig.setGenerateViaOptionsMinLength(15);
    pseudonymConfig.setGenerateViaOptionsMaxLength(15);
    com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition condition = new Condition();
    condition.setField("extension/valueString");
    condition.setOperator(ConditionOperator.EQUALS);
    condition.setType(ConditionType.STRING);
    condition.setValue("Asthma-Inhaler");
    List<ConditionalMaskRuleSet> maskRuleSet = new ArrayList<ConditionalMaskRuleSet>();
    ConditionalMaskRuleSet ruleSet1 = new ConditionalMaskRuleSet();
    ruleSet1.setCondition(condition);
    ruleSet1.setMaskingProvider(pseudonymConfig);
    conditionalMaskingProviderConfig.setMaskRuleSet(maskRuleSet);

    ConditionalMaskRuleSet ruleSet2 = new ConditionalMaskRuleSet();
    ruleSet2.setMaskingProvider(new HashMaskingProviderConfig());
    maskRuleSet.add(ruleSet1);
    maskRuleSet.add(ruleSet2);

    String utLine = "{" + "      \"resourceType\": \"Device\"," + "      \"id\": \"example\","
        + "      \"text\":{\"fhir_comments\":[\"text\"],\"status\":\"value_to_mask\"},"
        + "      \"active\": false," + "      \"status\": \"confirmed\","
        + "      \"version\": \"2.6\","
        + "      \"telecom\":[{\"system\":\"phone\", \"value\":\"123456789-987654321\"}],"
        + "      \"extension\":["
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName\", \"valueString\":\"Asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();

    ConditionalMaskingProvider maskingProvider =
        new ConditionalMaskingProvider(conditionalMaskingProviderConfig, tenantId,
            deidMaskingConfig, localizationProperty, new BasicMaskingProviderFactory());

    JsonNode nodeRoot = mapper.readTree(utLine);
    JsonNode nodeParent = nodeRoot.get("text");
    JsonNode inputDataNode = nodeParent.get("status");
    String identifier = inputDataNode.asText();
    MaskingActionInputIdentifier maii = new MaskingActionInputIdentifier(null, inputDataNode,
        nodeParent, "status", "", "", nodeRoot);
    maskingProvider.maskIdentifierBatch(Arrays.asList(maii));
    String maskedValue = maii.getRoot().get("text").get("status").asText();

    // System.out.println("\n=======>
    // testConditionalArrayFieldWithoutQueryIndex identifier: "
    // + identifier + " maskedValue: " + maskedValue);

    assertTrue(!maskedValue.equals(identifier));
    assertTrue(maskedValue.length() == 15);
  }

  /*
   * Test conditional operator equalsIgnoreCase
   */
  @Test
  public void testConditionalEqualsIgnoreCase() throws Exception {
    ConditionalMaskingProviderConfig conditionalMaskingProviderConfig =
        new ConditionalMaskingProviderConfig();
    PseudonymMaskingProviderConfig pseudonymConfig = new PseudonymMaskingProviderConfig();
    pseudonymConfig.setGenerateViaOptionsMinLength(15);
    pseudonymConfig.setGenerateViaOptionsMaxLength(15);
    com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition condition = new Condition();
    condition.setField(
        "extension/valueString(url==http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName)");
    condition.setOperator(ConditionOperator.EQUALS_IGNORE_CASE);
    condition.setType(ConditionType.STRING);
    condition.setValue("Asthma-Inhaler");
    List<ConditionalMaskRuleSet> maskRuleSet = new ArrayList<ConditionalMaskRuleSet>();
    ConditionalMaskRuleSet ruleSet1 = new ConditionalMaskRuleSet();
    ruleSet1.setCondition(condition);
    ruleSet1.setMaskingProvider(pseudonymConfig);
    conditionalMaskingProviderConfig.setMaskRuleSet(maskRuleSet);

    ConditionalMaskRuleSet ruleSet2 = new ConditionalMaskRuleSet();
    ruleSet2.setMaskingProvider(new HashMaskingProviderConfig());
    maskRuleSet.add(ruleSet1);
    maskRuleSet.add(ruleSet2);

    String utLine = "{" + "      \"resourceType\": \"Device\"," + "      \"id\": \"example\","
        + "      \"text\":{\"fhir_comments\":[\"text\"],\"status\":\"value_to_mask\"},"
        + "      \"active\": false," + "      \"status\": \"confirmed\","
        + "      \"version\": \"2.6\","
        + "      \"telecom\":[{\"system\":\"phone\", \"value\":\"123456789-987654321\"}],"
        + "      \"extension\":["
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName\", \"valueString\":\"asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();

    ConditionalMaskingProvider maskingProvider =
        new ConditionalMaskingProvider(conditionalMaskingProviderConfig, tenantId,
            deidMaskingConfig, localizationProperty, new BasicMaskingProviderFactory());

    JsonNode nodeRoot = mapper.readTree(utLine);
    JsonNode nodeParent = nodeRoot.get("text");
    JsonNode inputDataNode = nodeParent.get("status");
    String identifier = inputDataNode.asText();
    MaskingActionInputIdentifier maii = new MaskingActionInputIdentifier(null, inputDataNode,
        nodeParent, "status", "", "", nodeRoot);
    maskingProvider.maskIdentifierBatch(Arrays.asList(maii));
    String maskedValue = maii.getRoot().get("text").get("status").asText();
    // System.out.println("\n=======> testConditionalEqualsIgnoreCase
    // identifier: "
    // + identifier + " maskedValue: " + maskedValue);

    assertTrue(!maskedValue.equals(identifier));
    assertTrue(maskedValue.length() == 15);
  }

  /*
   * Test conditional operator contains
   */
  @Test
  public void testConditionalContains() throws Exception {
    ConditionalMaskingProviderConfig conditionalMaskingProviderConfig =
        new ConditionalMaskingProviderConfig();
    PseudonymMaskingProviderConfig pseudonymConfig = new PseudonymMaskingProviderConfig();
    pseudonymConfig.setGenerateViaOptionsMinLength(15);
    pseudonymConfig.setGenerateViaOptionsMaxLength(15);
    com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition condition = new Condition();
    condition.setField(
        "extension/valueString(url==http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName)");
    condition.setOperator(ConditionOperator.CONTAINS);
    condition.setType(ConditionType.STRING);
    condition.setValue("Inhaler");
    List<ConditionalMaskRuleSet> maskRuleSet = new ArrayList<ConditionalMaskRuleSet>();
    ConditionalMaskRuleSet ruleSet1 = new ConditionalMaskRuleSet();
    ruleSet1.setCondition(condition);
    ruleSet1.setMaskingProvider(pseudonymConfig);
    conditionalMaskingProviderConfig.setMaskRuleSet(maskRuleSet);

    ConditionalMaskRuleSet ruleSet2 = new ConditionalMaskRuleSet();
    ruleSet2.setMaskingProvider(new HashMaskingProviderConfig());
    maskRuleSet.add(ruleSet1);
    maskRuleSet.add(ruleSet2);

    String utLine = "{" + "      \"resourceType\": \"Device\"," + "      \"id\": \"example\","
        + "      \"text\":{\"fhir_comments\":[\"text\"],\"status\":\"value_to_mask\"},"
        + "      \"active\": false," + "      \"status\": \"confirmed\","
        + "      \"version\": \"2.6\","
        + "      \"telecom\":[{\"system\":\"phone\", \"value\":\"123456789-987654321\"}],"
        + "      \"extension\":["
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName\", \"valueString\":\"Asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();

    ConditionalMaskingProvider maskingProvider =
        new ConditionalMaskingProvider(conditionalMaskingProviderConfig, tenantId,
            deidMaskingConfig, localizationProperty, new BasicMaskingProviderFactory());

    JsonNode nodeRoot = mapper.readTree(utLine);
    JsonNode nodeParent = nodeRoot.get("text");
    JsonNode inputDataNode = nodeParent.get("status");
    String identifier = inputDataNode.asText();
    MaskingActionInputIdentifier maii = new MaskingActionInputIdentifier(null, inputDataNode,
        nodeParent, "status", "", "", nodeRoot);
    maskingProvider.maskIdentifierBatch(Arrays.asList(maii));
    String maskedValue = maii.getRoot().get("text").get("status").asText();

    assertTrue(!maskedValue.equals(identifier));
    assertTrue(maskedValue.length() == 15);
  }

  /*
   * Test conditional operator contained_in
   */
  @Test
  public void testConditionalContained_In() throws Exception {
    ConditionalMaskingProviderConfig conditionalMaskingProviderConfig =
        new ConditionalMaskingProviderConfig();
    PseudonymMaskingProviderConfig pseudonymConfig = new PseudonymMaskingProviderConfig();
    pseudonymConfig.setGenerateViaOptionsMinLength(15);
    pseudonymConfig.setGenerateViaOptionsMaxLength(15);
    com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition condition = new Condition();
    condition.setField(
        "extension/valueString(url==http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName)");
    condition.setOperator(ConditionOperator.CONTAINED_IN);
    condition.setType(ConditionType.STRING);
    condition.setValue("Asthma-Inhaler for Genral Use");
    List<ConditionalMaskRuleSet> maskRuleSet = new ArrayList<ConditionalMaskRuleSet>();
    ConditionalMaskRuleSet ruleSet1 = new ConditionalMaskRuleSet();
    ruleSet1.setCondition(condition);
    ruleSet1.setMaskingProvider(pseudonymConfig);
    conditionalMaskingProviderConfig.setMaskRuleSet(maskRuleSet);

    ConditionalMaskRuleSet ruleSet2 = new ConditionalMaskRuleSet();
    ruleSet2.setMaskingProvider(new HashMaskingProviderConfig());
    maskRuleSet.add(ruleSet1);
    maskRuleSet.add(ruleSet2);

    String utLine = "{" + "      \"resourceType\": \"Device\"," + "      \"id\": \"example\","
        + "      \"text\":{\"fhir_comments\":[\"text\"],\"status\":\"value_to_mask\"},"
        + "      \"active\": false," + "      \"status\": \"confirmed\","
        + "      \"version\": \"2.6\","
        + "      \"telecom\":[{\"system\":\"phone\", \"value\":\"123456789-987654321\"}],"
        + "      \"extension\":["
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName\", \"valueString\":\"Asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    ConditionalMaskingProvider maskingProvider =
        new ConditionalMaskingProvider(conditionalMaskingProviderConfig, tenantId,
            deidMaskingConfig, localizationProperty, new BasicMaskingProviderFactory());

    JsonNode nodeRoot = mapper.readTree(utLine);
    JsonNode nodeParent = nodeRoot.get("text");
    JsonNode inputDataNode = nodeParent.get("status");
    String identifier = inputDataNode.asText();
    MaskingActionInputIdentifier maii = new MaskingActionInputIdentifier(null, inputDataNode,
        nodeParent, "status", "", "", nodeRoot);
    maskingProvider.maskIdentifierBatch(Arrays.asList(maii));
    String maskedValue = maii.getRoot().get("text").get("status").asText();

    // System.out.println("\n=======> testConditionalContained_In
    // identifier: "
    // + identifier + " maskedValue: " + maskedValue);

    assertTrue(!maskedValue.equals(identifier));
    assertTrue(maskedValue.length() == 15);
  }

  /*
   * Test conditional operator anyOf
   */
  @Test
  public void testConditionalAnyOf() throws Exception {
    ConditionalMaskingProviderConfig conditionalMaskingProviderConfig =
        new ConditionalMaskingProviderConfig();
    RedactMaskingProviderConfig redactConfig = new RedactMaskingProviderConfig();
    com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition condition = new Condition();
    condition.setField("procedure/code/value");
    condition.setOperator(ConditionOperator.ANY_OF);
    condition.setType(ConditionType.STRING);
    condition.setValueList(Arrays.asList("201", "202", "203"));
    List<ConditionalMaskRuleSet> maskRuleSet = new ArrayList<>();
    ConditionalMaskRuleSet ruleSet1 = new ConditionalMaskRuleSet();
    ruleSet1.setCondition(condition);
    ruleSet1.setMaskingProvider(redactConfig);
    conditionalMaskingProviderConfig.setMaskRuleSet(maskRuleSet);
    maskRuleSet.add(ruleSet1);

    //@formatter:off
    String doc1 = "{\"text\": \"medical procedure\",\n"
        + "      \"procedure\":[{\n"
        + "           \"code\": {\n"
        + "               \"desc\": \"description of 101\",\n"
        + "               \"value\": \"101\"\n"
        + "           }},{\n"
        + "           \"code\": {\n"
        + "               \"desc\": \"description of 301\",\n"
        + "               \"value\": \"301\"\n"
        + "           }},{\n"        
        + "           \"code\": {\n"
        + "               \"desc\": \"null description\",\n"
        + "               \"value\": null\n"        
        + "           }},{\n"
        + "           \"codex\": {\n"
        + "               \"desc\": \"description of 302\",\n"
        + "               \"value\": \"302\"\n"
        + "           }},{\n"
        + "           \"code\": {\n"
        + "               \"desc\": \"description of 303\",\n"
        + "               \"Value\": \"303\"\n"
        + "           }},[1,3,4],{\n"
        + "           \"code\": {\n"
        + "               \"desc\": \"description of 201\",\n"
        + "               \"value\": \"201\"\n"
        + "           }}\n"
        + "       ] }\n";
    //@formatter:on

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode nodeRoot = mapper.readTree(doc1);
    JsonNode nodeParent = nodeRoot;
    JsonNode inputDataNode = nodeParent.get("text");
    MaskingActionInputIdentifier maii = new MaskingActionInputIdentifier(null, inputDataNode,
        nodeParent, "text", "", "", nodeRoot);

    //@formatter:off
    String doc2 = "{\"text\": \"second procedure\",\n"
        + "      \"procedure\":{\n"
        + "           \"code\": {\n"
        + "               \"value\": [\"701\",\"911\", {\"a\":1, \"b\":2}, \"abc\", \"\", null]\n"
        + "            }} }\n";
    //@formatter:on

    nodeRoot = mapper.readTree(doc2);
    nodeParent = nodeRoot;
    inputDataNode = nodeParent.get("text");
    MaskingActionInputIdentifier maii2 =
        new MaskingActionInputIdentifier(null, inputDataNode, nodeParent, "text", "", "", nodeRoot);

    TestConditionalMaskingProvider maskingProvider =
        new TestConditionalMaskingProvider(conditionalMaskingProviderConfig, tenantId,
            deidMaskingConfig, localizationProperty, new BasicMaskingProviderFactory());
    maskingProvider.maskIdentifierBatch(Arrays.asList(maii, maii2));

    String maskedValue = maii.getRoot().get("text").asText();
    assertEquals("XXXXXXXXXXXXXXXXX", maskedValue);
    Set<String> vs = maskingProvider.valueSets.get(0);
    assertTrue(vs.contains("101"));
    assertTrue(vs.contains("301"));
    assertTrue(vs.contains("201"));
    assertEquals(3, vs.size());

    maskedValue = maii2.getRoot().get("text").asText();
    assertEquals("second procedure", maskedValue);
    vs = maskingProvider.valueSets.get(1);
    assertTrue(vs.contains("701"));
    assertTrue(vs.contains("911"));
    assertTrue(vs.contains("abc"));
    assertTrue(vs.contains(""));
    assertEquals(4, vs.size());
  }

  /*
   * Test conditional operator notAnyOf
   */
  @Test
  public void testConditionalNotAnyOf() throws Exception {
    ConditionalMaskingProviderConfig conditionalMaskingProviderConfig =
        new ConditionalMaskingProviderConfig();
    RedactMaskingProviderConfig redactConfig = new RedactMaskingProviderConfig();
    com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition condition = new Condition();
    condition.setField("procedure/code/value");
    condition.setOperator(ConditionOperator.NOT_ANY_OF);
    condition.setType(ConditionType.STRING);
    condition.setValueList(Arrays.asList("201", "202", "203", "204"));
    List<ConditionalMaskRuleSet> maskRuleSet = new ArrayList<>();
    ConditionalMaskRuleSet ruleSet1 = new ConditionalMaskRuleSet();
    ruleSet1.setCondition(condition);
    ruleSet1.setMaskingProvider(redactConfig);
    conditionalMaskingProviderConfig.setMaskRuleSet(maskRuleSet);
    maskRuleSet.add(ruleSet1);

    //@formatter:off
    String doc1 = "{\"text\": \"medical procedure\",\n"
        + "      \"procedure\":[{\n"
        + "           \"code\": {\n"
        + "               \"desc\": \"description of 202\",\n"
        + "               \"value\": \"202\"\n"
        + "           }},{\n"
        + "           \"code\": {\n"
        + "               \"desc\": \"description of 203\",\n"
        + "               \"value\": \"203\"\n"
        + "           }},{\n"        
        + "           \"code\": {\n"
        + "               \"desc\": \"null description\",\n"
        + "               \"value\": null\n"        
        + "           }},{\n"
        + "           \"codex\": {\n"
        + "               \"desc\": \"description of 302\",\n"
        + "               \"value\": \"302\"\n"
        + "           }},{\n"
        + "           \"code\": {\n"
        + "               \"desc\": \"description of 303\",\n"
        + "               \"Value\": \"303\"\n"
        + "           }},[1,3,4],{\n"
        + "           \"code\": {\n"
        + "               \"desc\": \"description of 201\",\n"
        + "               \"value\": \"201\"\n"
        + "           }}\n"
        + "       ] }\n";
    //@formatter:on

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode nodeRoot = mapper.readTree(doc1);
    JsonNode nodeParent = nodeRoot;
    JsonNode inputDataNode = nodeParent.get("text");
    MaskingActionInputIdentifier maii =
        new MaskingActionInputIdentifier(null, inputDataNode, nodeParent, "text", "", "", nodeRoot);

    //@formatter:off
    String doc2 = "{\"text\": \"second procedure\",\n"
        + "      \"procedure\":{\n"
        + "           \"code\": {\n"
        + "               \"value\": [\"701\",\"911\", {\"a\":1, \"b\":2}, \"abc\", \"\", null]\n"
        + "            }} }\n";
    //@formatter:on

    nodeRoot = mapper.readTree(doc2);
    nodeParent = nodeRoot;
    inputDataNode = nodeParent.get("text");
    MaskingActionInputIdentifier maii2 =
        new MaskingActionInputIdentifier(null, inputDataNode, nodeParent, "text", "", "", nodeRoot);

    TestConditionalMaskingProvider maskingProvider =
        new TestConditionalMaskingProvider(conditionalMaskingProviderConfig, tenantId,
            deidMaskingConfig, localizationProperty, new BasicMaskingProviderFactory());
    maskingProvider.maskIdentifierBatch(Arrays.asList(maii, maii2));

    Set<String> vs = maskingProvider.valueSets.get(0);
    assertTrue(vs.contains("201"));
    assertTrue(vs.contains("202"));
    assertTrue(vs.contains("203"));
    assertEquals(3, vs.size());
    String maskedValue = maii.getRoot().get("text").asText();
    assertEquals("medical procedure", maskedValue);

    vs = maskingProvider.valueSets.get(1);
    assertTrue(vs.contains("701"));
    assertTrue(vs.contains("911"));
    assertTrue(vs.contains("abc"));
    assertTrue(vs.contains(""));
    assertEquals(4, vs.size());
    maskedValue = maii2.getRoot().get("text").asText();
    assertEquals("XXXXXXXXXXXXXXXX", maskedValue);
  }

  /*
   * Test conditional operator AnyOfIgnoreCase
   */
  @Test
  public void testConditionalAnyOfIgnoreCase() throws Exception {
    ConditionalMaskingProviderConfig conditionalMaskingProviderConfig =
        new ConditionalMaskingProviderConfig();
    RedactMaskingProviderConfig redactConfig = new RedactMaskingProviderConfig();
    com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition condition = new Condition();
    condition.setField("procedure/code/value");
    condition.setOperator(ConditionOperator.ANY_OF_IGNORE_CASE);
    condition.setType(ConditionType.STRING);
    condition.setValueList(Arrays.asList("Abc", "202", "Def"));
    List<ConditionalMaskRuleSet> maskRuleSet = new ArrayList<>();
    ConditionalMaskRuleSet ruleSet1 = new ConditionalMaskRuleSet();
    ruleSet1.setCondition(condition);
    ruleSet1.setMaskingProvider(redactConfig);
    conditionalMaskingProviderConfig.setMaskRuleSet(maskRuleSet);
    maskRuleSet.add(ruleSet1);

    //@formatter:off
    String doc1 = "{\"text\": \"medical procedure\",\n"
        + "      \"procedure\":[{\n"
        + "           \"code\": {\n"
        + "               \"desc\": \"description of 101\",\n"
        + "               \"value\": \"101\"\n"
        + "           }},{\n"
        + "           \"code\": {\n"
        + "               \"desc\": \"description of 301\",\n"
        + "               \"value\": \"301\"\n"
        + "           }},{\n"        
        + "           \"code\": {\n"
        + "               \"desc\": \"null description\",\n"
        + "               \"value\": null\n"        
        + "           }},{\n"
        + "           \"codex\": {\n"
        + "               \"desc\": \"description of 302\",\n"
        + "               \"value\": \"302\"\n"
        + "           }},{\n"
        + "           \"code\": {\n"
        + "               \"desc\": \"description of 303\",\n"
        + "               \"Value\": \"303\"\n"
        + "           }},[1,3,4],{\n"
        + "           \"code\": {\n"
        + "               \"desc\": \"description of 201\",\n"
        + "               \"value\": \"201\"\n"
        + "           }}\n"
        + "       ] }\n";
    //@formatter:on

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode nodeRoot = mapper.readTree(doc1);
    JsonNode nodeParent = nodeRoot;
    JsonNode inputDataNode = nodeParent.get("text");
    MaskingActionInputIdentifier maii =
        new MaskingActionInputIdentifier(null, inputDataNode, nodeParent, "text", "", "", nodeRoot);

    //@formatter:off
    String doc2 = "{\"text\": \"second procedure\",\n"
        + "      \"procedure\":{\n"
        + "           \"code\": {\n"
        + "               \"value\": [\"701\",\"911\", {\"a\":1, \"b\":2}, \"abc\", \"\", null]\n"
        + "            }} }\n";
    //@formatter:on

    nodeRoot = mapper.readTree(doc2);
    nodeParent = nodeRoot;
    inputDataNode = nodeParent.get("text");
    MaskingActionInputIdentifier maii2 =
        new MaskingActionInputIdentifier(null, inputDataNode, nodeParent, "text", "", "", nodeRoot);

    TestConditionalMaskingProvider maskingProvider =
        new TestConditionalMaskingProvider(conditionalMaskingProviderConfig, tenantId,
            deidMaskingConfig, localizationProperty, new BasicMaskingProviderFactory());
    maskingProvider.maskIdentifierBatch(Arrays.asList(maii, maii2));

    String maskedValue = maii.getRoot().get("text").asText();
    assertEquals("medical procedure", maskedValue);
    Set<String> vs = maskingProvider.valueSets.get(0);
    assertTrue(vs.contains("101"));
    assertTrue(vs.contains("301"));
    assertTrue(vs.contains("201"));
    assertEquals(3, vs.size());

    maskedValue = maii2.getRoot().get("text").asText();
    assertEquals("XXXXXXXXXXXXXXXX", maskedValue);
    vs = maskingProvider.valueSets.get(1);
    assertTrue(vs.contains("701"));
    assertTrue(vs.contains("911"));
    assertTrue(vs.contains("abc"));
    assertTrue(vs.contains(""));
    assertEquals(4, vs.size());
  }

  /*
   * Test conditional operator NotAnyOfIgnoreCase
   */
  @Test
  public void testConditionalNotAnyOfIgnoreCase() throws Exception {
    ConditionalMaskingProviderConfig conditionalMaskingProviderConfig =
        new ConditionalMaskingProviderConfig();
    RedactMaskingProviderConfig redactConfig = new RedactMaskingProviderConfig();
    com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition condition = new Condition();
    condition.setField("procedure/code/value");
    condition.setOperator(ConditionOperator.NOT_ANY_OF_IGNORE_CASE);
    condition.setType(ConditionType.STRING);
    condition.setValueList(Arrays.asList("Abc", "202", "Def"));
    List<ConditionalMaskRuleSet> maskRuleSet = new ArrayList<>();
    ConditionalMaskRuleSet ruleSet1 = new ConditionalMaskRuleSet();
    ruleSet1.setCondition(condition);
    ruleSet1.setMaskingProvider(redactConfig);
    conditionalMaskingProviderConfig.setMaskRuleSet(maskRuleSet);
    maskRuleSet.add(ruleSet1);

    //@formatter:off
    String doc1 = "{\"text\": \"medical procedure\",\n"
        + "      \"procedure\":[{\n"
        + "           \"code\": {\n"
        + "               \"desc\": \"description of 1ABC\",\n"
        + "               \"value\": \"1ABC\"\n"
        + "           }},{\n"
        + "           \"code\": {\n"
        + "               \"desc\": \"description of 202\",\n"
        + "               \"value\": \"202\"\n"
        + "           }},{\n"        
        + "           \"code\": {\n"
        + "               \"desc\": \"null description\",\n"
        + "               \"value\": null\n"        
        + "           }},{\n"
        + "           \"codex\": {\n"
        + "               \"desc\": \"description of 302\",\n"
        + "               \"value\": \"302\"\n"
        + "           }},{\n"
        + "           \"code\": {\n"
        + "               \"desc\": \"description of 303\",\n"
        + "               \"Value\": \"abc\"\n"
        + "           }},[1,3,4],{\n"
        + "           \"code\": {\n"
        + "               \"desc\": \"description of def\",\n"
        + "               \"value\": \"Def\"\n"
        + "           }}\n"
        + "       ] }\n";
    //@formatter:on

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode nodeRoot = mapper.readTree(doc1);
    JsonNode nodeParent = nodeRoot;
    JsonNode inputDataNode = nodeParent.get("text");
    MaskingActionInputIdentifier maii =
        new MaskingActionInputIdentifier(null, inputDataNode, nodeParent, "text", "", "", nodeRoot);

    //@formatter:off
    String doc2 = "{\"text\": \"second procedure\",\n"
        + "      \"procedure\":{\n"
        + "           \"code\": {\n"
        + "               \"value\": [\"ABC\",\"def\", {\"a\":1, \"b\":2}, \"202\", null]\n"
        + "            }} }\n";
    //@formatter:on

    nodeRoot = mapper.readTree(doc2);
    nodeParent = nodeRoot;
    inputDataNode = nodeParent.get("text");
    MaskingActionInputIdentifier maii2 =
        new MaskingActionInputIdentifier(null, inputDataNode, nodeParent, "text", "", "", nodeRoot);

    TestConditionalMaskingProvider maskingProvider =
        new TestConditionalMaskingProvider(conditionalMaskingProviderConfig, tenantId,
            deidMaskingConfig, localizationProperty, new BasicMaskingProviderFactory());
    maskingProvider.maskIdentifierBatch(Arrays.asList(maii, maii2));

    Set<String> vs = maskingProvider.valueSets.get(0);
    assertTrue(vs.contains("1ABC")); // value that causes the match on not any of
    assertTrue(vs.contains("202"));
    assertTrue(vs.contains("Def"));
    assertEquals(3, vs.size());
    String maskedValue = maii.getRoot().get("text").asText();
    assertEquals("XXXXXXXXXXXXXXXXX", maskedValue);

    vs = maskingProvider.valueSets.get(1);
    assertTrue(vs.contains("ABC"));
    assertTrue(vs.contains("def"));
    assertTrue(vs.contains("202"));
    assertEquals(3, vs.size());
    maskedValue = maii2.getRoot().get("text").asText();
    assertEquals("second procedure", maskedValue);
  }

  /*
   * Test conditional with regular path is NOT MET and the hash is not specified, therefore the data
   * is not masked
   */
  @Test
  public void testConditionalNotMetRegularPath_DataNotMasked() throws Exception {
    ConditionalMaskingProviderConfig conditionalMaskingProviderConfig =
        new ConditionalMaskingProviderConfig();
    PseudonymMaskingProviderConfig pseudonymConfig = new PseudonymMaskingProviderConfig();
    pseudonymConfig.setGenerateViaOptionsMinLength(12);
    pseudonymConfig.setGenerateViaOptionsMaxLength(12);
    com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition condition = new Condition();
    condition.setField("text/status_NOt_MET");
    condition.setOperator(ConditionOperator.EQUALS);
    condition.setType(ConditionType.STRING);
    condition.setValue("mask_condition");
    List<ConditionalMaskRuleSet> maskRuleSet = new ArrayList<ConditionalMaskRuleSet>();
    ConditionalMaskRuleSet ruleSet1 = new ConditionalMaskRuleSet();
    ruleSet1.setCondition(condition);
    ruleSet1.setMaskingProvider(pseudonymConfig);
    conditionalMaskingProviderConfig.setMaskRuleSet(maskRuleSet);

    maskRuleSet.add(ruleSet1);

    String utLine = "{" + "      \"resourceType\": \"Device\"," + "      \"id\": \"example\","
        + "      \"text\":{\"fhir_comments\":[\"text\"],\"status\":\"mask_condition\"},"
        + "      \"active\": false," + "      \"status\": \"confirmed\","
        + "      \"version\": \"2.6\","
        + "      \"telecom\":[{\"system\":\"phone\", \"value\":\"123456789-987654321\"}],"
        + "      \"extension\":["
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName\", \"valueString\":\"Asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    ConditionalMaskingProvider maskingProvider =
        new ConditionalMaskingProvider(conditionalMaskingProviderConfig, tenantId,
            deidMaskingConfig, localizationProperty, new BasicMaskingProviderFactory());

    JsonNode nodeRoot = mapper.readTree(utLine);
    JsonNode nodeParent = nodeRoot.get("text");
    JsonNode inputDataNode = nodeParent.get("status");
    String identifier = inputDataNode.asText();
    MaskingActionInputIdentifier maii = new MaskingActionInputIdentifier(null, inputDataNode,
        nodeParent, "status", "", "", nodeRoot);
    maskingProvider.maskIdentifierBatch(Arrays.asList(maii));
    String maskedValue = maii.getRoot().get("text").get("status").asText();

    // System.out.println("\n=======>
    // testConditionalNotMetRegularPath_DataNotMasked identifier: "
    // + identifier + " maskedValue: " + maskedValue);

    // Since condition with regular path is not met, the input data is not
    // masked
    assertTrue(maskedValue.equals(identifier));
  }

  /*
   * Test conditional with regular path is NOT MET and the default hash is specified, therefore the
   * input data is hashed.
   */
  @Test
  public void testConditionalNotMetRegularPath_WithDefaultHash() throws Exception {
    ConditionalMaskingProviderConfig conditionalMaskingProviderConfig =
        new ConditionalMaskingProviderConfig();
    PseudonymMaskingProviderConfig pseudonymConfig = new PseudonymMaskingProviderConfig();
    pseudonymConfig.setGenerateViaOptionsMinLength(12);
    pseudonymConfig.setGenerateViaOptionsMaxLength(12);
    com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition condition = new Condition();
    condition.setField("text_NO_MET/status\\");
    condition.setOperator(ConditionOperator.EQUALS);
    condition.setType(ConditionType.STRING);
    condition.setValue("mask_condition");
    List<ConditionalMaskRuleSet> maskRuleSet = new ArrayList<ConditionalMaskRuleSet>();
    ConditionalMaskRuleSet ruleSet1 = new ConditionalMaskRuleSet();
    ruleSet1.setCondition(condition);
    ruleSet1.setMaskingProvider(pseudonymConfig);
    conditionalMaskingProviderConfig.setMaskRuleSet(maskRuleSet);

    ConditionalMaskRuleSet ruleSet2 = new ConditionalMaskRuleSet();
    ruleSet2.setMaskingProvider(new HashMaskingProviderConfig());
    maskRuleSet.add(ruleSet1);
    maskRuleSet.add(ruleSet2);

    String utLine = "{" + "      \"resourceType\": \"Device\"," + "      \"id\": \"example\","
        + "      \"text\":{\"fhir_comments\":[\"text\"],\"status\":\"value_to_mask\"},"
        + "      \"active\": false," + "      \"status\": \"confirmed\","
        + "      \"version\": \"2.6\","
        + "      \"telecom\":[{\"system\":\"phone\", \"value\":\"123456789-987654321\"}],"
        + "      \"extension\":["
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName\", \"valueString\":\"Asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    ConditionalMaskingProvider maskingProvider =
        new ConditionalMaskingProvider(conditionalMaskingProviderConfig, tenantId,
            deidMaskingConfig, localizationProperty, new BasicMaskingProviderFactory());

    JsonNode nodeRoot = mapper.readTree(utLine);
    JsonNode nodeParent = nodeRoot.get("text");
    JsonNode inputDataNode = nodeParent.get("status");
    MaskingActionInputIdentifier maii = new MaskingActionInputIdentifier(null, inputDataNode,
        nodeParent, "status", "", "", nodeRoot);
    maskingProvider.maskIdentifierBatch(Arrays.asList(maii));
    String maskedValue = maii.getRoot().get("text").get("status").asText();

    // System.out.println("\n=======>
    // testConditionalNotMetRegularPath_WithDefaultHash identifier: "
    // + identifier + " maskedValue: " + maskedValue);
    // The condition with regular path is not met, but the default hash
    // condition is specified, the input data is hashed.
    assertTrue(
        maskedValue.equals("7182557138CC2E22BD350BE5100B5C2C78F06BA0393A8099B792CEBACD184AD4"));
  }

  /*
   * Test conditional with array condition is NOT MET and the default hash condition is not
   * specified, therefore the data is not masked.
   */
  @Test
  public void testConditionalNotMetArrayNoMask() throws Exception {
    ConditionalMaskingProviderConfig conditionalMaskingProviderConfig =
        new ConditionalMaskingProviderConfig();
    PseudonymMaskingProviderConfig pseudonymConfig = new PseudonymMaskingProviderConfig();
    pseudonymConfig.setGenerateViaOptionsMinLength(15);
    pseudonymConfig.setGenerateViaOptionsMaxLength(15);
    com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition condition = new Condition();
    condition.setField(
        "extension_NOT_MET/valueString(url==http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName)");
    condition.setOperator(ConditionOperator.CONTAINED_IN);
    condition.setType(ConditionType.STRING);
    condition.setValue("Asthma-Inhaler for Genral Use");
    List<ConditionalMaskRuleSet> maskRuleSet = new ArrayList<ConditionalMaskRuleSet>();
    ConditionalMaskRuleSet ruleSet1 = new ConditionalMaskRuleSet();
    ruleSet1.setCondition(condition);
    ruleSet1.setMaskingProvider(pseudonymConfig);
    conditionalMaskingProviderConfig.setMaskRuleSet(maskRuleSet);
    maskRuleSet.add(ruleSet1);

    String utLine = "{" + "      \"resourceType\": \"Device\"," + "      \"id\": \"example\","
        + "      \"text\":{\"fhir_comments\":[\"text\"],\"status\":\"mask_condition\"},"
        + "      \"active\": false," + "      \"status\": \"confirmed\","
        + "      \"version\": \"2.6\","
        + "      \"telecom\":[{\"system\":\"phone\", \"value\":\"123456789-987654321\"}],"
        + "      \"extension\":["
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName\", \"valueString\":\"Asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    ConditionalMaskingProvider maskingProvider =
        new ConditionalMaskingProvider(conditionalMaskingProviderConfig, tenantId,
            deidMaskingConfig, localizationProperty, new BasicMaskingProviderFactory());

    JsonNode nodeRoot = mapper.readTree(utLine);
    JsonNode nodeParent = nodeRoot.get("text");
    JsonNode inputDataNode = nodeParent.get("status");
    String identifier = inputDataNode.asText();
    MaskingActionInputIdentifier maii = new MaskingActionInputIdentifier(null, inputDataNode,
        nodeParent, "status", "", "", nodeRoot);
    maskingProvider.maskIdentifierBatch(Arrays.asList(maii));
    String maskedValue = maii.getRoot().get("text").get("status").asText();

    // System.out.println("\n=======> testConditionalNotMetArrayNoMask
    // identifier: "
    // + identifier + " maskedValue: " + maskedValue);

    // Since condition is not met, the input data is not masked
    assertTrue(maskedValue.equals(identifier));
  }

  /*
   * Test conditional with array condition is NOT MET, but the default hash condition is specified,
   * therefore the input data is hashed.
   */
  @Test
  public void testConditionalNotMetArray_WithDefaultHash() throws Exception {
    ConditionalMaskingProviderConfig conditionalMaskingProviderConfig =
        new ConditionalMaskingProviderConfig();
    PseudonymMaskingProviderConfig pseudonymConfig = new PseudonymMaskingProviderConfig();
    pseudonymConfig.setGenerateViaOptionsMinLength(15);
    pseudonymConfig.setGenerateViaOptionsMaxLength(15);
    com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition condition = new Condition();
    condition.setField(
        "extension_NOT_MET/valueString(url==http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName)");
    condition.setOperator(ConditionOperator.CONTAINED_IN);
    condition.setType(ConditionType.STRING);
    condition.setValue("Asthma-Inhaler for Genral Use");
    List<ConditionalMaskRuleSet> maskRuleSet = new ArrayList<ConditionalMaskRuleSet>();
    ConditionalMaskRuleSet ruleSet1 = new ConditionalMaskRuleSet();
    ruleSet1.setCondition(condition);
    ruleSet1.setMaskingProvider(pseudonymConfig);
    conditionalMaskingProviderConfig.setMaskRuleSet(maskRuleSet);

    ConditionalMaskRuleSet ruleSet2 = new ConditionalMaskRuleSet();
    ruleSet2.setMaskingProvider(new HashMaskingProviderConfig());
    maskRuleSet.add(ruleSet1);
    maskRuleSet.add(ruleSet2);

    String utLine = "{" + "      \"resourceType\": \"Device\"," + "      \"id\": \"example\","
        + "      \"text\":{\"fhir_comments\":[\"text\"],\"status\":\"value_to_mask\"},"
        + "      \"active\": false," + "      \"status\": \"confirmed\","
        + "      \"version\": \"2.6\","
        + "      \"telecom\":[{\"system\":\"phone\", \"value\":\"123456789-987654321\"}],"
        + "      \"extension\":["
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName\", \"valueString\":\"Asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();

    ConditionalMaskingProvider maskingProvider =
        new ConditionalMaskingProvider(conditionalMaskingProviderConfig, tenantId,
            deidMaskingConfig, localizationProperty, new BasicMaskingProviderFactory());

    JsonNode nodeRoot = mapper.readTree(utLine);
    JsonNode nodeParent = nodeRoot.get("text");
    JsonNode inputDataNode = nodeParent.get("status");
    MaskingActionInputIdentifier maii = new MaskingActionInputIdentifier(null, inputDataNode,
        nodeParent, "status", "", "", nodeRoot);
    maskingProvider.maskIdentifierBatch(Arrays.asList(maii));
    String maskedValue = maii.getRoot().get("text").get("status").asText();

    // System.out.println("\n=======> testConditionalNotMetDefaultHash
    // identifier: "
    // + identifier + " maskedValue: " + maskedValue);

    // The array condition is not met, but the default hash condition is
    // specified, the input data is hashed
    assertTrue(
        maskedValue.equals("7182557138CC2E22BD350BE5100B5C2C78F06BA0393A8099B792CEBACD184AD4"));
  }

  /*
   * Test conditional exception from constructor.
   */

  /*
   * Test conditional field missing.
   */
  @Test(expected = JsonMappingException.class)
  public void testConditionalMissingField()
      throws JsonParseException, JsonMappingException, IOException {
    String maskingOptionValue = "[{\"default.masking.provider\": \"PSEUDONYM\","
        + "\"pseudonym.generateViaOptions.minLength\": 15,"
        + "\"pseudonym.generateViaOptions.maxLength\": 15," + "\"condition\" : {"
        + "\"field_MISSING\":\"extension/valueString(url==http://www.ibm.com/watsonhealth/fhir/extensions/test-data/r1/resourceName)\","
        + "\"operator\" : \"equals\", " + "\"type\" : \"string\", "
        + "\"value\" : \"Asthma-Inhaler for Genral Use\"} " + "},"
        + "{\"default.masking.provider\": \"HASH\"} " + "]";

    try {
      ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
      mapper.readValue(maskingOptionValue, ConditionalMaskingProviderConfig.class);

    } catch (JsonMappingException e) {
      assertTrue(e.getMessage().contains("need JSON String that contains type id"));
      throw e;
    }
  }

  /*
   * Test conditional invalid type value.
   */

  @Test(expected = JsonMappingException.class)
  public void testConditionalInvalidType()
      throws JsonParseException, JsonMappingException, IOException {
    String maskingOptionValue =
        "{\"type\":\"CONDITIONAL\",\"unexpectedInputHandling\":\"NULL\",\"unexpectedInputReturnMessage\":\"OTHER\",\"maskRuleSet\":[{\"condition\":{\"field\":\"text/status\",\"operator\":\"equals\",\"type\":\"INVALID_TYPE\",\"value\":\"mask_condition\"},\"maskingProvider\":{\"type\":\"PSEUDONYM\",\"unexpectedInputHandling\":\"NULL\",\"unexpectedInputReturnMessage\":\"OTHER\",\"generateViaOptionsEnabled\":true,\"generateViaOptionsMinLength\":12,\"generateViaOptionsMaxLength\":12,\"generateViaOptionsGenerateUppercase\":true,\"generateViaOptionsGenerateLowercase\":true,\"generateViaOptionsGenerateDigit\":true,\"generateViaOptionsGenerateSpecial\":false,\"generateViaPatternEnabled\":true,\"generateViaPatternLanguageCode\":\"EN\",\"generateViaHashEnabled\":false,\"generateViaHashUseSHA256\":false}},{\"maskingProvider\":{\"type\":\"HASH\",\"unexpectedInputHandling\":\"NULL\",\"unexpectedInputReturnMessage\":\"OTHER\",\"algorithmDefault\":\"SHA-256\",\"offsetOffsetMask\":false,\"offsetOffsetMaskDelete\":false,\"offsetBegin\":-1,\"offsetEnd\":-1,\"salt\":\"\",\"offsetInvalidOffsetValue\":1}}]}";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    try {
      mapper.readValue(maskingOptionValue, ConditionalMaskingProviderConfig.class);
    } catch (JsonMappingException e) {
      assertTrue(e.getMessage().contains("ConditionType` from String \"INVALID_TYPE\""));
      throw e;
    }
  }

  /*
   * Test conditional invalid operator value.
   */
  @Test(expected = JsonMappingException.class)
  public void testConditionalInvalidOperator()
      throws JsonParseException, JsonMappingException, IOException {
    String maskingOptionValue =
        "{\"type\":\"CONDITIONAL\",\"unexpectedInputHandling\":\"NULL\",\"unexpectedInputReturnMessage\":\"OTHER\",\"maskRuleSet\":[{\"condition\":{\"field\":\"text/status\",\"operator\":\"INVALID_OPERATOR\",\"type\":\"string\",\"value\":\"mask_condition\"},\"maskingProvider\":{\"type\":\"PSEUDONYM\",\"unexpectedInputHandling\":\"NULL\",\"unexpectedInputReturnMessage\":\"OTHER\",\"generateViaOptionsEnabled\":true,\"generateViaOptionsMinLength\":12,\"generateViaOptionsMaxLength\":12,\"generateViaOptionsGenerateUppercase\":true,\"generateViaOptionsGenerateLowercase\":true,\"generateViaOptionsGenerateDigit\":true,\"generateViaOptionsGenerateSpecial\":false,\"generateViaPatternEnabled\":true,\"generateViaPatternLanguageCode\":\"EN\",\"generateViaHashEnabled\":false,\"generateViaHashUseSHA256\":false}},{\"maskingProvider\":{\"type\":\"HASH\",\"unexpectedInputHandling\":\"NULL\",\"unexpectedInputReturnMessage\":\"OTHER\",\"algorithmDefault\":\"SHA-256\",\"offsetOffsetMask\":false,\"offsetOffsetMaskDelete\":false,\"offsetBegin\":-1,\"offsetEnd\":-1,\"salt\":\"\",\"offsetInvalidOffsetValue\":1}}]}";

    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    try {
      mapper.readValue(maskingOptionValue, ConditionalMaskingProviderConfig.class);
    } catch (JsonMappingException e) {
      assertTrue(e.getMessage().contains(
          "String \"INVALID_OPERATOR\": not one of the values accepted for Enum class: ["));
      throw e;
    }
  }
  
  /*
   * Test conditional invalid operator value.
   */
  @Test
  public void testGetConditionArrayFieldValue() throws IOException {
    // @formatter_off
    String data = "{" +
        "\"resourceType\": \"PatientX\"," +
        "\"gender\": [" +
        "                   {\"system\": \"f1\",  \"value\": \"yarg1\"}," +
        "                   {\"system\": \"f2\",  \"value\": \"yarg2\"}," +
        "                   {\"system\": null,  \"value\": \"yarg2\"}," +
        "                   {\"system\": \"f1\",  \"value\": null}," +
        "                   {\"system\": \"f1\",  \"valueX\": \"y3\"}," +
        "                   {\"system\": 1,  \"valueX\": \"y3\"}," +
        "                   [6,7,8]," +
        "                   null," +
        "                   \"first\"," +
        "                   {\"systemx\": 1,  \"valueX\": \"y3\"}," +
        "                   {\"system\": \"f1\",  \"value\": \"male\"}" +
        "        ]," +
        "\"birthDate\": \"1920-02-04\"" +
        "}";
    // @formatter_on
    
    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    JsonNode root = mapper.readTree(data);

    Condition condition = new Condition();
    condition.setField("/gender/value(system==f1)");
    condition.setOperator(ConditionOperator.EQUALS);
    condition.setValue("v2");
    
    ConditionalMaskingProvider provider = new ConditionalMaskingProvider(new ConditionalMaskingProviderConfig(), null, null, null, null);
    
    Set<String> valueSet = provider.getConditionArrayFieldValue(condition, root, null);
    assertNotNull(valueSet);
    assertTrue(valueSet.toString(), valueSet.contains("yarg1"));
    assertTrue(valueSet.contains("male"));
    assertEquals(2, valueSet.size());
  }
}
