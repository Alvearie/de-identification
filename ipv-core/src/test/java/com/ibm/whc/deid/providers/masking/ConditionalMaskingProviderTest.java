/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.Test;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.providers.masking.fhir.FHIRMaskingUtils;
import com.ibm.whc.deid.providers.masking.fhir.MaskingActionInputIdentifier;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.ConditionalMaskRuleSet;
import com.ibm.whc.deid.shared.pojo.config.masking.ConditionalMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.HashMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.PseudonymMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.conditional.Condition;
import com.ibm.whc.deid.shared.pojo.config.masking.conditional.ConditionOperator;
import com.ibm.whc.deid.shared.pojo.config.masking.conditional.ConditionType;
import com.ibm.whc.deid.shared.util.ConfigGenerator;

public class ConditionalMaskingProviderTest {

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
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName\", \"valueString\":\"Asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = FHIRMaskingUtils.getObjectMapper();
    System.out.println(mapper.writeValueAsString(conditionalMaskingProviderConfig));

    ConditionalMaskingProvider maskingProvider = new ConditionalMaskingProvider(
        conditionalMaskingProviderConfig, tenantId, deidMaskingConfig);

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
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName\", \"valueString\":\"Asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = FHIRMaskingUtils.getObjectMapper();

    ConditionalMaskingProvider maskingProvider = new ConditionalMaskingProvider(
        conditionalMaskingProviderConfig, tenantId, deidMaskingConfig);

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
        "extension/valueString(url==http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName)");
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
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName\", \"valueString\":\"asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = FHIRMaskingUtils.getObjectMapper();

    ConditionalMaskingProvider maskingProvider = new ConditionalMaskingProvider(
        conditionalMaskingProviderConfig, tenantId, deidMaskingConfig);

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
        "extension/valueString(url==http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName)");
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
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName\", \"valueString\":\"Asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = FHIRMaskingUtils.getObjectMapper();

    ConditionalMaskingProvider maskingProvider = new ConditionalMaskingProvider(
        conditionalMaskingProviderConfig, tenantId, deidMaskingConfig);

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
        "extension/valueString(url==http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName)");
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
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName\", \"valueString\":\"Asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = FHIRMaskingUtils.getObjectMapper();
    ConditionalMaskingProvider maskingProvider = new ConditionalMaskingProvider(
        conditionalMaskingProviderConfig, tenantId, deidMaskingConfig);

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
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName\", \"valueString\":\"Asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = FHIRMaskingUtils.getObjectMapper();
    ConditionalMaskingProvider maskingProvider = new ConditionalMaskingProvider(
        conditionalMaskingProviderConfig, tenantId, deidMaskingConfig);

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
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName\", \"valueString\":\"Asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = FHIRMaskingUtils.getObjectMapper();
    ConditionalMaskingProvider maskingProvider = new ConditionalMaskingProvider(
        conditionalMaskingProviderConfig, tenantId, deidMaskingConfig);

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
        "extension_NOT_MET/valueString(url==http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName)");
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
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName\", \"valueString\":\"Asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = FHIRMaskingUtils.getObjectMapper();
    ConditionalMaskingProvider maskingProvider = new ConditionalMaskingProvider(
        conditionalMaskingProviderConfig, tenantId, deidMaskingConfig);

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
        "extension_NOT_MET/valueString(url==http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName)");
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
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName\", \"valueString\":\"Asthma-Inhaler\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/requestID\", \"valueString\":\"Req_ID\"},"
        + "      {\"url\":\"http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/messageID\", \"valueString\":\"Msg_ID\"}]"
        + "    }";

    // Define JSON data to parse
    ObjectMapper mapper = FHIRMaskingUtils.getObjectMapper();

    ConditionalMaskingProvider maskingProvider = new ConditionalMaskingProvider(
        conditionalMaskingProviderConfig, tenantId, deidMaskingConfig);

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
        + "\"field_MISSING\":\"extension/valueString(url==http://www.ibm.com/watsonhealth/fhir/extensions/whc-lsf/r1/resourceName)\","
        + "\"operator\" : \"equals\", " + "\"type\" : \"string\", "
        + "\"value\" : \"Asthma-Inhaler for Genral Use\"} " + "},"
        + "{\"default.masking.provider\": \"HASH\"} " + "]";

    try {
      ObjectMapper mapper = FHIRMaskingUtils.getObjectMapper();
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
        "{\"type\":\"CONDITIONAL\",\"unspecifiedValueHandling\":0,\"unspecifiedValueReturnMessage\":\"OTHER\",\"maskRuleSet\":[{\"condition\":{\"field\":\"text/status\",\"operator\":\"equals\",\"type\":\"INVALID_TYPE\",\"value\":\"mask_condition\"},\"maskingProvider\":{\"type\":\"PSEUDONYM\",\"unspecifiedValueHandling\":0,\"unspecifiedValueReturnMessage\":\"OTHER\",\"generateViaOptionsEnabled\":true,\"generateViaOptionsMinLength\":12,\"generateViaOptionsMaxLength\":12,\"generateViaOptionsGenerateUppercase\":true,\"generateViaOptionsGenerateLowercase\":true,\"generateViaOptionsGenerateDigit\":true,\"generateViaOptionsGenerateSpecial\":false,\"generateViaPatternEnabled\":true,\"generateViaPatternLanguageCode\":\"EN\",\"generateViaHashEnabled\":false,\"generateViaHashUseSHA256\":false}},{\"maskingProvider\":{\"type\":\"HASH\",\"unspecifiedValueHandling\":0,\"unspecifiedValueReturnMessage\":\"OTHER\",\"algorithmDefault\":\"SHA-256\",\"offsetOffsetMask\":false,\"offsetOffsetMaskDelete\":false,\"offsetBegin\":-1,\"offsetEnd\":-1,\"salt\":\"\",\"offsetInvalidOffsetValue\":1}}]}";

    ObjectMapper mapper = FHIRMaskingUtils.getObjectMapper();
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
        "{\"type\":\"CONDITIONAL\",\"unspecifiedValueHandling\":0,\"unspecifiedValueReturnMessage\":\"OTHER\",\"maskRuleSet\":[{\"condition\":{\"field\":\"text/status\",\"operator\":\"INVALID_OPERATOR\",\"type\":\"string\",\"value\":\"mask_condition\"},\"maskingProvider\":{\"type\":\"PSEUDONYM\",\"unspecifiedValueHandling\":0,\"unspecifiedValueReturnMessage\":\"OTHER\",\"generateViaOptionsEnabled\":true,\"generateViaOptionsMinLength\":12,\"generateViaOptionsMaxLength\":12,\"generateViaOptionsGenerateUppercase\":true,\"generateViaOptionsGenerateLowercase\":true,\"generateViaOptionsGenerateDigit\":true,\"generateViaOptionsGenerateSpecial\":false,\"generateViaPatternEnabled\":true,\"generateViaPatternLanguageCode\":\"EN\",\"generateViaHashEnabled\":false,\"generateViaHashUseSHA256\":false}},{\"maskingProvider\":{\"type\":\"HASH\",\"unspecifiedValueHandling\":0,\"unspecifiedValueReturnMessage\":\"OTHER\",\"algorithmDefault\":\"SHA-256\",\"offsetOffsetMask\":false,\"offsetOffsetMaskDelete\":false,\"offsetBegin\":-1,\"offsetEnd\":-1,\"salt\":\"\",\"offsetInvalidOffsetValue\":1}}]}";

    ObjectMapper mapper = FHIRMaskingUtils.getObjectMapper();
    try {
      mapper.readValue(maskingOptionValue, ConditionalMaskingProviderConfig.class);
    } catch (JsonMappingException e) {
      assertTrue(e.getMessage().contains(
          "String \"INVALID_OPERATOR\": value not one of declared Enum instance names: [containedIn, equalsIgnoreCase, contains, equals]"));
      throw e;
    }
  }
}
