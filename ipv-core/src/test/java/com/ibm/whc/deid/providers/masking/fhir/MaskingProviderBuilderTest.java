/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.Ignore;
import org.junit.Test;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.providers.masking.BasicMaskingProviderFactory;
import com.ibm.whc.deid.providers.masking.MaskingProviderFactory;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.Rule;
import com.ibm.whc.deid.shared.pojo.config.json.JsonConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateDependencyMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.MaintainMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.NullMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.ConfigGenerator;
import com.ibm.whc.deid.shared.util.MaskingConfigUtils;

/**
 * Unit tests for generic masking provider
 *
 */
public class MaskingProviderBuilderTest {

  private MaskingProviderFactory maskingProviderFactory = new BasicMaskingProviderFactory();

  private String schemaType = "fhir";

  private final boolean arrayAllRules = true;
  private final boolean defNoRuleRes = true;
  private final String tenantId = "TEST_TENANT";

  private final String hashRuleName = "hashRule";

  private final String maintainRuleName = "maintainRule";

  private final String deleteRuleName = "deleteRule";

  private final String randomRuleName = "randomRule";

  private final String datetimeRuleName = "datetimeRule";

  private final String cityRuleName = "cityRule";

  @Test
  public void testBasicMaskDeleteSimple() throws Exception {
    Map<String, String> deviceMaskConf = new HashMap<>();
    deviceMaskConf.put("/fhir/Device/note", "note");
    deviceMaskConf.put("/fhir/Device/expiry", "expiry");

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Device", deviceMaskConf);

    ObjectMapper objectMapper = new ObjectMapper();

    JsonNode device;
    try (InputStream in = getClass().getResourceAsStream("/fhir/deviceExample.json")) {
      device = objectMapper.readTree(in);
    }

    assertTrue(device.get("expiry").isTextual());

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedDevice = genericMaskingProvider.mask(device);

    JsonNode expiryNode = maskedDevice.get("expiry");
    assertNull(expiryNode.asText());
  }

  /**
   * Add rules for testing
   *
   * @param testMaskingConfig
   */
  private DeidMaskingConfig addRules(DeidMaskingConfig testMaskingConfig) {
    MaskingProviderConfig config =
        MaskingProviderConfig.getDefaultMaskingProviderConfig(MaskingProviderType.HASH);
    Rule hashRule = MaskingConfigUtils.createRuleWithOneProvider(hashRuleName, config);

    MaskingProviderConfig randomConfig =
        MaskingProviderConfig.getDefaultMaskingProviderConfig(MaskingProviderType.RANDOM);
    Rule randomRule = MaskingConfigUtils.createRuleWithOneProvider(randomRuleName, randomConfig);

    MaskingProviderConfig cityConfig =
        MaskingProviderConfig.getDefaultMaskingProviderConfig(MaskingProviderType.CITY);
    Rule cityRule = MaskingConfigUtils.createRuleWithOneProvider(cityRuleName, cityConfig);
    cityConfig.setUnspecifiedValueHandling(1);

    DateTimeMaskingProviderConfig datetimeConfig =
        (DateTimeMaskingProviderConfig) MaskingProviderConfig
            .getDefaultMaskingProviderConfig(MaskingProviderType.DATETIME);
    datetimeConfig.setGeneralizeMonthyear(true);
    Rule datetimeRule =
        MaskingConfigUtils.createRuleWithOneProvider(datetimeRuleName, datetimeConfig);

    NullMaskingProviderConfig deleteConfig = (NullMaskingProviderConfig) MaskingProviderConfig
        .getDefaultMaskingProviderConfig(MaskingProviderType.NULL);
    Rule deleteRule = MaskingConfigUtils.createRuleWithOneProvider(deleteRuleName, deleteConfig);

    MaintainMaskingProviderConfig maintainConfig =
        (MaintainMaskingProviderConfig) MaskingProviderConfig
            .getDefaultMaskingProviderConfig(MaskingProviderType.MAINTAIN);
    Rule maintainRule =
        MaskingConfigUtils.createRuleWithOneProvider(maintainRuleName, maintainConfig);

    List<Rule> rules = testMaskingConfig.getRules();
    rules.add(hashRule);
    rules.add(randomRule);
    rules.add(datetimeRule);
    rules.add(cityRule);
    rules.add(deleteRule);
    rules.add(maintainRule);

    testMaskingConfig.setRules(rules);

    return testMaskingConfig;
  }

  @Test
  public void testArrayMaskHash_NoIndex() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/identifier/value", hashRuleName);

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    String originalReference = "ID1";
    assertEquals(originalReference, group.get("identifier").get(1).get("value").asText());

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);
    JsonNode maskedValue = genericMaskingProvider.mask(group);

    String maskedExpect0 = "74F3E37238EEEB6E9FFC731106D53C689FED464D1BDF3633C9A247D942DDD8D9";
    assertEquals(maskedExpect0, maskedValue.get("identifier").get(0).get("value").asText());

    // assertNotEquals(originalReference,
    // maskedValue.get("identifier").get(1).get("value").asText());
    String maskedExpect1 = "38DBA034F94A84CD77EBFD04AC4B11DAAC185207DC9DCC313B91B9172796ED3F";
    assertEquals(maskedExpect1, maskedValue.get("identifier").get(1).get("value").asText());

    String maskedExpect2 = "5179C2D2C2995F87598ACF637439A179914637F5CCDCC3561C5D1A0743A8061A";
    assertEquals(maskedExpect2, maskedValue.get("identifier").get(2).get("value").asText());

    // Test other values unchanged
    assertEquals("ID0", maskedValue.get("identifier").get(0).get("system").asText());
    assertEquals("ID1", maskedValue.get("identifier").get(1).get("system").asText());
    assertEquals("ID2", maskedValue.get("identifier").get(2).get("system").asText());
  }

  @Test
  public void testArrayMaskDelete_NoIndex() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/identifier/value", deleteRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    String originalReference = "ID1";
    assertEquals(originalReference, group.get("identifier").get(1).get("value").asText());

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    // Check that the specified item was successfully deleted and the rest weren't
    JsonNode maskedValue = genericMaskingProvider.mask(group);

    assertNull(maskedValue.get("identifier").get(1).get("value").asText());
    assertNotNull(maskedValue.get("identifier").get(1).get("system").asText());
    assertNull(maskedValue.get("identifier").get(0).get("value").asText());
    assertNotNull(maskedValue.get("identifier").get(0).get("system").asText());
    assertNull(maskedValue.get("identifier").get(2).get("value").asText());
    assertNotNull(maskedValue.get("identifier").get(2).get("system").asText());
  }

  @Ignore
  @Test
  public void testDefNoRulesRes_flag() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/id", maintainRuleName);
    groupMaskConf.put("/fhir/Group/resourceType", maintainRuleName);
    groupMaskConf.put("/fhir/Group/identifier[0]/type/coding[1]/code", maintainRuleName);
    groupMaskConf.put("/fhir/Group/identifier[0]/type/coding[0]/name/first", maintainRuleName);
    groupMaskConf.put("/fhir/Group/identifier[1]/type/coding[0]/code", hashRuleName);
    groupMaskConf.put("/fhir/Group/extension[0]/extension[0]/url", maintainRuleName);
    groupMaskConf.put("/fhir/Group/extension[0]/extension[0]/valueString", maintainRuleName);
    groupMaskConf.put("/fhir/Group/extension[0]/extension[1]/valueString", maintainRuleName);
    groupMaskConf.put("/fhir/Group/extension[1]/extension[0]/valueString", hashRuleName);
    groupMaskConf.put("/fhir/Group/extension[1]/extension[1]/valueString", hashRuleName);
    groupMaskConf.put("/fhir/Group/identifier[1]/type/coding[1]", maintainRuleName);
    groupMaskConf.put("/fhir/Group/identifier[1]/type/coding[1]", maintainRuleName);
    groupMaskConf.put("/fhir/Group/identifier[1]/type/coding[1]/system", deleteRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    ObjectMapper objectMapper = new ObjectMapper();
    JsonNode group = objectMapper
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example2.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);
    JsonConfig jsonConfig = testMaskingConfig.getJson();
    jsonConfig.addMaskingRule("/identifier/value", hashRuleName);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    genericMaskingProvider.setDefNoRuleRes(false);

    // Check that the specified item was successfully deleted and the rest
    // weren't
    JsonNode maskedValue = genericMaskingProvider.mask(group);

    objectMapper.writerWithDefaultPrettyPrinter().writeValue(System.out, maskedValue);
    assertEquals("101", maskedValue.get("id").asText());
    assertEquals("Group", maskedValue.get("resourceType").asText());
    assertEquals("TestURL.0.0",
        maskedValue.get("extension").get(0).get("extension").get(0).get("url").asText());
    assertEquals("TestString.0.0",
        maskedValue.get("extension").get(0).get("extension").get(0).get("valueString").asText());
    assertEquals("TestString.0.1",
        maskedValue.get("extension").get(0).get("extension").get(1).get("valueString").asText());
    assertEquals("john", maskedValue.get("identifier").get(0).get("type").get("coding").get(0)
        .get("name").get(0).get("first").asText());
    assertEquals("lalafirst", maskedValue.get("identifier").get(0).get("type").get("coding").get(0)
        .get("name").get(1).get("first").asText());
    assertEquals("2F98DA7420CFB4E1517BA040511AF23CF529711F4CB8A6746A453105E442A28D",
        maskedValue.get("identifier").get(1).get("type").get("coding").get(0).get("code").asText());
    assertEquals("0DD3C918ADB16575C47F4D8B27862D86EE67BD5EFB2BAFDA2AEBF661254A21A1",
        maskedValue.get("extension").get(1).get("extension").get(0).get("valueString").asText());
    assertEquals("88CAFB31A719642B7DA0ADB63E72C015B4DCC66C4247D4EAC8CED064A8360C06",
        maskedValue.get("extension").get(1).get("extension").get(1).get("valueString").asText());
    assertNull(maskedValue.get("code"));
    assertNull(maskedValue.get("quantity"));
  }

  @Test
  public void testNestedArrayMaskHash_NoIndexNN() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/extension/extension/valueString", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    String originalReference = "TestString.1.1";
    assertEquals(originalReference,
        group.get("extension").get(1).get("extension").get(1).get("valueString").asText());

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValue = genericMaskingProvider.mask(group);

    String maskedExpect00 = "10EFDE3500F99326821F384D2FD878D019B9C7E658001B07F8F491D63697C820";
    assertEquals(maskedExpect00,
        maskedValue.get("extension").get(0).get("extension").get(0).get("valueString").asText());

    String maskedExpect01 = "9DB7D558482AF53B524217A5F4A69DE1C6F78A213782F1A41C20711B3155E4E1";
    assertEquals(maskedExpect01,
        maskedValue.get("extension").get(0).get("extension").get(1).get("valueString").asText());

    String maskedExpect10 = "0DD3C918ADB16575C47F4D8B27862D86EE67BD5EFB2BAFDA2AEBF661254A21A1";
    assertEquals(maskedExpect10,
        maskedValue.get("extension").get(1).get("extension").get(0).get("valueString").asText());

    String maskedExpect11 = "88CAFB31A719642B7DA0ADB63E72C015B4DCC66C4247D4EAC8CED064A8360C06";
    assertEquals(maskedExpect11,
        maskedValue.get("extension").get(1).get("extension").get(1).get("valueString").asText());

    // Test the other values are unchanged
    assertEquals("TestURL.0.0",
        maskedValue.get("extension").get(0).get("extension").get(0).get("url").asText());
    assertEquals("TestURL.0.1",
        maskedValue.get("extension").get(0).get("extension").get(1).get("url").asText());
    assertEquals("TestURL.1.0",
        maskedValue.get("extension").get(1).get("extension").get(0).get("url").asText());
    assertEquals("TestURL.1.1",
        maskedValue.get("extension").get(1).get("extension").get(1).get("url").asText());
  }

  @Test
  public void testArrayMaskHash_Values_NoIndex() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/array", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);
    // Check that the specified item was successfully masked
    JsonNode maskedValue = genericMaskingProvider.mask(group);

    String maskedExpect0 = "FD601A88D32FF3CC5FD5508E617841F6FBAADDDFE5B21241F1CF9AC3F80BF272";
    String maskedExpect1 = "3C9683017F9E4BF33D0FBEDD26BF143FD72DE9B9DD145441B75F0604047EA28E";
    String maskedExpect2 = "0537D481F73A757334328052DA3AF9626CED97028E20B849F6115C22CD765197";
    assertEquals(maskedExpect0, maskedValue.get("array").get(0).asText());
    assertEquals(maskedExpect1, maskedValue.get("array").get(1).asText());
    assertEquals(maskedExpect2, maskedValue.get("array").get(2).asText());
  }

  @Test
  public void testArrayMaskDelete_Values_NoIndex() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/array", deleteRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    // Check that the specified item was successfully masked
    JsonNode maskedValue = genericMaskingProvider.mask(group);

    assertNull(maskedValue.get("array").get(0).asText());
    assertNull(maskedValue.get("array").get(1).asText());
    assertNull(maskedValue.get("array").get(2).asText());
  }

  @Test
  public void testBasicMaskSimpleMaskingProvider() throws Exception {
    Map<String, String> deviceMaskConf = new HashMap<>();
    deviceMaskConf.put("/fhir/Device/expiry", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Device", deviceMaskConf);

    JsonNode device = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/deviceExample.json"));

    String originalReference = "2020-08-08";

    assertEquals(originalReference, device.get("expiry").asText());

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedDevice = genericMaskingProvider.mask(device);
    assertNotEquals(originalReference, maskedDevice.get("expiry").asText());
  }

  // Tests masking a normal element as an array. Should keep original value
  @Test
  public void testBasicMaskSimpleMaskingProvider_Invalid() throws Exception {
    Map<String, String> deviceMaskConf = new HashMap<>();
    deviceMaskConf.put("/fhir/Device/expiry[*]", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Device", deviceMaskConf);

    JsonNode device = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/deviceExample.json"));

    String originalReference = "2020-08-08";

    assertEquals(originalReference, device.get("expiry").asText());

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedDevice = genericMaskingProvider.mask(device);
    assertEquals(originalReference, maskedDevice.get("expiry").asText());
  }

  // This calls MaskingProviderBuilder.maskFinalPathComplex
  @Ignore
  @Test
  public void testBasicMaskDateDependency() throws Exception {
    String dateDependencyRuleName = "dateDependency";
    Map<String, String> patientMaskConf = new HashMap<>();
    patientMaskConf.put("/", dateDependencyRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Patient", patientMaskConf);

    String patientJSON = "{\n" + " \"resourceType\": \"Patient\",\n" + " \"id\": \"example\",\n"
        + " \"active\": false,\n" + " \"birthDate\": \"08-12-2010 00:02:00\",\n"
        + " \"deceasedBoolean\": true,\n" + " \"deceasedDateTime\": \"05-11-2011 00:00:00\"\n"
        + " }";

    ObjectMapper mapper = new ObjectMapper();
    JsonNode patient = mapper.readTree(patientJSON);

    DateDependencyMaskingProviderConfig maskingConfiguration =
        new DateDependencyMaskingProviderConfig();

    maskingConfiguration.setDatetimeYearDeleteNIntervalMaskDate("birthDate");
    maskingConfiguration.setDatetimeYearDeleteNIntervalCompareDate("deceasedDateTime");

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();

    Rule dateRule =
        MaskingConfigUtils.createRuleWithOneProvider(dateDependencyRuleName, maskingConfiguration);

    List<Rule> rules = testMaskingConfig.getRules();
    rules.add(dateRule);

    testMaskingConfig.setRules(rules);

    JsonConfig jsonConfig = testMaskingConfig.getJson();
    jsonConfig.addMaskingRule("/identifier/value", hashRuleName);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedPatient = genericMaskingProvider.mask(patient);
    assertTrue(maskedPatient.get("birthDate").asText().equals("08/12"));
    assertTrue(maskedPatient.get("deceasedDateTime").asText().equals("05-11-2011 00:00:00"));
  }

  @Test
  public void testBasicMaskHashOnce() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/type", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValue = genericMaskingProvider.mask(group);

    // Value if hashed once
    String maskedOnce = "AB91A82D14CFC6125C1A40E6920A4CD3FCC3E2F9940347175B06367845A9BCBC";

    // Everything was hashed
    assertEquals(maskedOnce, maskedValue.get("type").asText());
  }

  @Test
  public void testArrayMaskHash_Index1() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/identifier[1]/value", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    String originalReference = "ID1";
    assertEquals(originalReference, group.get("identifier").get(1).get("value").asText());

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    // Check that the specified item was successfully masked
    JsonNode maskedValue = genericMaskingProvider.mask(group);
    String maskedExpect1 = "38DBA034F94A84CD77EBFD04AC4B11DAAC185207DC9DCC313B91B9172796ED3F";
    assertEquals(maskedExpect1, maskedValue.get("identifier").get(1).get("value").asText());

    // Check that the other values are unchanged
    assertEquals("ID0", maskedValue.get("identifier").get(0).get("value").asText());
    assertEquals("ID0", maskedValue.get("identifier").get(0).get("system").asText());
    assertEquals("ID1", maskedValue.get("identifier").get(1).get("system").asText());
    assertEquals("ID2", maskedValue.get("identifier").get(2).get("value").asText());
    assertEquals("ID2", maskedValue.get("identifier").get(2).get("system").asText());
  }

  @Test
  public void testArrayMaskDelete_Index1() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/identifier[1]/value", deleteRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    String originalReference = "ID1";
    assertEquals(originalReference, group.get("identifier").get(1).get("value").asText());

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    // Check that the specified item was successfully deleted and the rest weren't
    JsonNode maskedValue = genericMaskingProvider.mask(group);
    assertNull(maskedValue.get("identifier").get(1).get("value").asText());
    assertNotNull(maskedValue.get("identifier").get(1).get("system").asText());
    assertNotNull(maskedValue.get("identifier").get(0).get("value").asText());
    assertNotNull(maskedValue.get("identifier").get(0).get("system").asText());
    assertNotNull(maskedValue.get("identifier").get(2).get("value").asText());
    assertNotNull(maskedValue.get("identifier").get(2).get("system").asText());
  }

  @Test
  public void testArrayMaskHash_IndexN() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/identifier[*]/value", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    String originalReference = "ID1";
    assertEquals(originalReference, group.get("identifier").get(1).get("value").asText());

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValue = genericMaskingProvider.mask(group);

    String maskedExpect0 = "74F3E37238EEEB6E9FFC731106D53C689FED464D1BDF3633C9A247D942DDD8D9";
    assertEquals(maskedExpect0, maskedValue.get("identifier").get(0).get("value").asText());

    // assertNotEquals(originalReference,
    // maskedValue.get("identifier").get(1).get("value").asText());
    String maskedExpect1 = "38DBA034F94A84CD77EBFD04AC4B11DAAC185207DC9DCC313B91B9172796ED3F";
    assertEquals(maskedExpect1, maskedValue.get("identifier").get(1).get("value").asText());

    String maskedExpect2 = "5179C2D2C2995F87598ACF637439A179914637F5CCDCC3561C5D1A0743A8061A";
    assertEquals(maskedExpect2, maskedValue.get("identifier").get(2).get("value").asText());

    // Test other values unchanged
    assertEquals("ID0", maskedValue.get("identifier").get(0).get("system").asText());
    assertEquals("ID1", maskedValue.get("identifier").get(1).get("system").asText());
    assertEquals("ID2", maskedValue.get("identifier").get(2).get("system").asText());
  }

  @Test
  public void testArrayMaskDelete_IndexN() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/identifier[*]/value", deleteRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    String originalReference = "ID1";
    assertEquals(originalReference, group.get("identifier").get(1).get("value").asText());

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    // Check that the specified item was successfully deleted and the rest weren't
    JsonNode maskedValue = genericMaskingProvider.mask(group);
    assertNull(maskedValue.get("identifier").get(1).get("value").asText());
    assertNotNull(maskedValue.get("identifier").get(1).get("system").asText());
    assertNull(maskedValue.get("identifier").get(0).get("value").asText());
    assertNotNull(maskedValue.get("identifier").get(0).get("system").asText());
    assertNull(maskedValue.get("identifier").get(2).get("value").asText());
    assertNotNull(maskedValue.get("identifier").get(2).get("system").asText());
  }

  @Test
  public void testArrayMaskHash_Values1() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/array[1]", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    // Check that the specified item was successfully masked
    JsonNode maskedValue = genericMaskingProvider.mask(group);

    String maskedExpect1 = "3C9683017F9E4BF33D0FBEDD26BF143FD72DE9B9DD145441B75F0604047EA28E";
    assertEquals(maskedExpect1, maskedValue.get("array").get(1).asText());

    // Check that the other values are unchanged
    assertEquals("value0", maskedValue.get("array").get(0).asText());
    assertEquals("value2", maskedValue.get("array").get(2).asText());
  }

  @Test
  public void testArrayMaskHash_ValuesN() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/array[*]", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    // Check that the specified item was successfully masked
    JsonNode maskedValue = genericMaskingProvider.mask(group);

    String maskedExpect0 = "FD601A88D32FF3CC5FD5508E617841F6FBAADDDFE5B21241F1CF9AC3F80BF272";
    String maskedExpect1 = "3C9683017F9E4BF33D0FBEDD26BF143FD72DE9B9DD145441B75F0604047EA28E";
    String maskedExpect2 = "0537D481F73A757334328052DA3AF9626CED97028E20B849F6115C22CD765197";
    assertEquals(maskedExpect0, maskedValue.get("array").get(0).asText());
    assertEquals(maskedExpect1, maskedValue.get("array").get(1).asText());
    assertEquals(maskedExpect2, maskedValue.get("array").get(2).asText());
  }

  @Test
  public void testArrayMaskHash_ValuesRange() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/array[1,2]", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    // Check that the specified item was successfully masked
    JsonNode maskedValue = genericMaskingProvider.mask(group);

    String maskedExpect1 = "3C9683017F9E4BF33D0FBEDD26BF143FD72DE9B9DD145441B75F0604047EA28E";
    String maskedExpect2 = "0537D481F73A757334328052DA3AF9626CED97028E20B849F6115C22CD765197";
    assertEquals(maskedExpect1, maskedValue.get("array").get(1).asText());
    assertEquals(maskedExpect2, maskedValue.get("array").get(2).asText());

    // Check that the other values are unchanged
    assertEquals("value0", maskedValue.get("array").get(0).asText());
  }

  @Test
  public void testArrayMaskHash_ValuesList() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/array[{0,2}]", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    // Check that the specified item was successfully masked
    JsonNode maskedValue = genericMaskingProvider.mask(group);

    String maskedExpect0 = "FD601A88D32FF3CC5FD5508E617841F6FBAADDDFE5B21241F1CF9AC3F80BF272";
    String maskedExpect2 = "0537D481F73A757334328052DA3AF9626CED97028E20B849F6115C22CD765197";
    assertEquals(maskedExpect0, maskedValue.get("array").get(0).asText());
    assertEquals(maskedExpect2, maskedValue.get("array").get(2).asText());

    // Check that the other values are unchanged
    assertEquals("value1", maskedValue.get("array").get(1).asText());
  }

  @Test
  public void testArrayMaskDelete_Values1() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/array[1]", deleteRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    // Check that the specified item was successfully masked
    JsonNode maskedValue = genericMaskingProvider.mask(group);

    assertNotNull(maskedValue.get("array").get(0).asText());
    assertNull(maskedValue.get("array").get(1).asText());
    assertNotNull(maskedValue.get("array").get(2).asText());
  }

  @Test
  public void testArrayMaskDelete_ValuesN() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/array[*]", deleteRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    // Check that the specified item was successfully masked
    JsonNode maskedValue = genericMaskingProvider.mask(group);

    assertNull(maskedValue.get("array").get(0).asText());
    assertNull(maskedValue.get("array").get(1).asText());
    assertNull(maskedValue.get("array").get(2).asText());
  }

  @Test
  public void testArrayMaskDelete_ValuesRange() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/array[1,2]", deleteRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    // Check that the specified item was successfully masked
    JsonNode maskedValue = genericMaskingProvider.mask(group);

    assertNotNull(maskedValue.get("array").get(0).asText());
    assertNull(maskedValue.get("array").get(1).asText());
    assertNull(maskedValue.get("array").get(2).asText());
  }

  @Test
  public void testArrayMaskDelete_ValuesList() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/array[{0,2}]", deleteRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    // Check that the specified item was successfully masked
    JsonNode maskedValue = genericMaskingProvider.mask(group);

    assertNull(maskedValue.get("array").get(0).asText());
    assertNotNull(maskedValue.get("array").get(1).asText());
    assertNull(maskedValue.get("array").get(2).asText());
  }

  @Test
  public void testNestedArrayHash_Index11() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/extension[1]/extension[1]/valueString", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    String originalReference = "TestString.1.1";

    assertEquals(originalReference,
        group.get("extension").get(1).get("extension").get(1).get("valueString").asText());

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValue = genericMaskingProvider.mask(group);

    String maskedExpect11 = "88CAFB31A719642B7DA0ADB63E72C015B4DCC66C4247D4EAC8CED064A8360C06";
    assertEquals(maskedExpect11,
        maskedValue.get("extension").get(1).get("extension").get(1).get("valueString").asText());
    assertEquals("TestString.0.0",
        maskedValue.get("extension").get(0).get("extension").get(0).get("valueString").asText());
    assertEquals("TestString.0.1",
        maskedValue.get("extension").get(0).get("extension").get(1).get("valueString").asText());
    assertEquals("TestString.1.0",
        maskedValue.get("extension").get(1).get("extension").get(0).get("valueString").asText());
  }

  @Test
  public void testNestedArrayMaskHash_Index1N() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/extension[1]/extension[*]/valueString", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    String originalReference = "TestString.1.1";
    assertEquals(originalReference,
        group.get("extension").get(1).get("extension").get(1).get("valueString").asText());

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValue = genericMaskingProvider.mask(group);

    String maskedExpect11 = "88CAFB31A719642B7DA0ADB63E72C015B4DCC66C4247D4EAC8CED064A8360C06";
    assertEquals(maskedExpect11,
        maskedValue.get("extension").get(1).get("extension").get(1).get("valueString").asText());

    String maskedExpect10 = "0DD3C918ADB16575C47F4D8B27862D86EE67BD5EFB2BAFDA2AEBF661254A21A1";
    assertEquals(maskedExpect10,
        maskedValue.get("extension").get(1).get("extension").get(0).get("valueString").asText());

    assertEquals("TestString.0.0",
        maskedValue.get("extension").get(0).get("extension").get(0).get("valueString").asText());
    assertEquals("TestString.0.1",
        maskedValue.get("extension").get(0).get("extension").get(1).get("valueString").asText());
  }

  @Test
  public void testNestedArrayMaskRandom_Index1N() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/extension[1]/extension[*]/valueString", randomRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    String originalReference = "TestString.1.1";
    assertEquals(originalReference,
        group.get("extension").get(1).get("extension").get(1).get("valueString").asText());

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    String maskedExpect11 = "88CAFB31A719642B7DA0ADB63E72C015B4DCC66C4247D4EAC8CED064A8360C06";
    String maskedExpect10 = "0DD3C918ADB16575C47F4D8B27862D86EE67BD5EFB2BAFDA2AEBF661254A21A1";

    // Check that output not equal to hashed nor is equal to original
    JsonNode maskedValue = genericMaskingProvider.mask(group);
    assertEquals("TestString.0.0",
        maskedValue.get("extension").get(0).get("extension").get(0).get("valueString").asText());
    assertEquals("TestString.0.1",
        maskedValue.get("extension").get(0).get("extension").get(1).get("valueString").asText());
    assertNotEquals(maskedExpect10,
        maskedValue.get("extension").get(1).get("extension").get(0).get("valueString").asText());
    assertNotEquals(maskedExpect11,
        maskedValue.get("extension").get(1).get("extension").get(1).get("valueString").asText());
    assertNotEquals("TestString.1.0",
        maskedValue.get("extension").get(1).get("extension").get(0).get("valueString").asText());
    assertNotEquals("TestString.1.1",
        maskedValue.get("extension").get(1).get("extension").get(1).get("valueString").asText());
    assertEquals("TestURL.1.1",
        maskedValue.get("extension").get(1).get("extension").get(1).get("url").asText());
  }

  @Test
  public void testNestedArrayMaskDate_Index1N() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/extension[1]/extension[*]/valueDateTime", datetimeRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    String originalReference = "TestString.1.1";
    assertEquals(originalReference,
        group.get("extension").get(1).get("extension").get(1).get("valueString").asText());

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);
    JsonNode maskedValue = genericMaskingProvider.mask(group);
    assertEquals("07/2017",
        maskedValue.get("extension").get(1).get("extension").get(0).get("valueDateTime").asText());
    assertEquals("12/1990",
        maskedValue.get("extension").get(1).get("extension").get(1).get("valueDateTime").asText());
    assertEquals("2017-07-01",
        maskedValue.get("extension").get(0).get("extension").get(1).get("valueDateTime").asText());
  }

  @Test
  public void testNestedArrayMaskHash_IndexN1() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/extension[*]/extension[1]/valueString", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);
    JsonConfig jsonConfig = testMaskingConfig.getJson();
    jsonConfig.addMaskingRule("/identifier/value", hashRuleName);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValue = genericMaskingProvider.mask(group);

    String maskedExpect01 = "9DB7D558482AF53B524217A5F4A69DE1C6F78A213782F1A41C20711B3155E4E1";
    assertEquals(maskedExpect01,
        maskedValue.get("extension").get(0).get("extension").get(1).get("valueString").asText());

    String maskedExpect11 = "88CAFB31A719642B7DA0ADB63E72C015B4DCC66C4247D4EAC8CED064A8360C06";
    assertEquals(maskedExpect11,
        maskedValue.get("extension").get(1).get("extension").get(1).get("valueString").asText());

    // Check that extension[*]/extension[0] stayed the same
    String maskedExpect10 = "0DD3C918ADB16575C47F4D8B27862D86EE67BD5EFB2BAFDA2AEBF661254A21A1";
    assertNotEquals(maskedExpect10,
        maskedValue.get("extension").get(1).get("extension").get(0).get("valueString").asText());
    assertEquals("TestString.0.0",
        maskedValue.get("extension").get(0).get("extension").get(0).get("valueString").asText());
  }

  @Test
  public void testNestedArrayMaskHash_IndexNN() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/extension[*]/extension[*]/valueString", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    String originalReference = "TestString.1.1";
    assertEquals(originalReference,
        group.get("extension").get(1).get("extension").get(1).get("valueString").asText());

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);
    JsonConfig jsonConfig = testMaskingConfig.getJson();
    jsonConfig.addMaskingRule("/identifier/value", hashRuleName);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValue = genericMaskingProvider.mask(group);

    String maskedExpect00 = "10EFDE3500F99326821F384D2FD878D019B9C7E658001B07F8F491D63697C820";
    assertEquals(maskedExpect00,
        maskedValue.get("extension").get(0).get("extension").get(0).get("valueString").asText());

    String maskedExpect01 = "9DB7D558482AF53B524217A5F4A69DE1C6F78A213782F1A41C20711B3155E4E1";
    assertEquals(maskedExpect01,
        maskedValue.get("extension").get(0).get("extension").get(1).get("valueString").asText());

    String maskedExpect10 = "0DD3C918ADB16575C47F4D8B27862D86EE67BD5EFB2BAFDA2AEBF661254A21A1";
    assertEquals(maskedExpect10,
        maskedValue.get("extension").get(1).get("extension").get(0).get("valueString").asText());

    String maskedExpect11 = "88CAFB31A719642B7DA0ADB63E72C015B4DCC66C4247D4EAC8CED064A8360C06";
    assertEquals(maskedExpect11,
        maskedValue.get("extension").get(1).get("extension").get(1).get("valueString").asText());

    // Test the other values are unchanged
    assertEquals("TestURL.0.0",
        maskedValue.get("extension").get(0).get("extension").get(0).get("url").asText());
    assertEquals("TestURL.0.1",
        maskedValue.get("extension").get(0).get("extension").get(1).get("url").asText());
    assertEquals("TestURL.1.0",
        maskedValue.get("extension").get(1).get("extension").get(0).get("url").asText());
    assertEquals("TestURL.1.1",
        maskedValue.get("extension").get(1).get("extension").get(1).get("url").asText());
  }

  @Test
  public void testNestedArrayMaskHash_NoIndexN1() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/extension/extension[1]/valueString", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    String originalReference = "TestString.1.1";
    assertEquals(originalReference,
        group.get("extension").get(1).get("extension").get(1).get("valueString").asText());

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);
    JsonConfig jsonConfig = testMaskingConfig.getJson();
    jsonConfig.addMaskingRule("/identifier/value", hashRuleName);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);
    JsonNode maskedValue = genericMaskingProvider.mask(group);

    String maskedExpect01 = "9DB7D558482AF53B524217A5F4A69DE1C6F78A213782F1A41C20711B3155E4E1";
    assertEquals(maskedExpect01,
        maskedValue.get("extension").get(0).get("extension").get(1).get("valueString").asText());
    String maskedExpect11 = "88CAFB31A719642B7DA0ADB63E72C015B4DCC66C4247D4EAC8CED064A8360C06";
    assertEquals(maskedExpect11,
        maskedValue.get("extension").get(1).get("extension").get(1).get("valueString").asText());

    // Test the other values are unchanged
    assertEquals("TestString.0.0",
        maskedValue.get("extension").get(0).get("extension").get(0).get("valueString").asText());
    assertEquals("TestString.1.0",
        maskedValue.get("extension").get(1).get("extension").get(0).get("valueString").asText());
    assertEquals("TestURL.0.0",
        maskedValue.get("extension").get(0).get("extension").get(0).get("url").asText());
    assertEquals("TestURL.0.1",
        maskedValue.get("extension").get(0).get("extension").get(1).get("url").asText());
    assertEquals("TestURL.1.0",
        maskedValue.get("extension").get(1).get("extension").get(0).get("url").asText());
    assertEquals("TestURL.1.1",
        maskedValue.get("extension").get(1).get("extension").get(1).get("url").asText());
  }

  @Test
  public void testNestedArrayMaskHashTwice() throws Exception {
    // Expected Values
    String maskedExpect00 = "10EFDE3500F99326821F384D2FD878D019B9C7E658001B07F8F491D63697C820";
    String maskedExpect01 = "9DB7D558482AF53B524217A5F4A69DE1C6F78A213782F1A41C20711B3155E4E1";
    String maskedExpect10 = "0DD3C918ADB16575C47F4D8B27862D86EE67BD5EFB2BAFDA2AEBF661254A21A1";
    String maskedExpect11 = "88CAFB31A719642B7DA0ADB63E72C015B4DCC66C4247D4EAC8CED064A8360C06";

    String maskedExpect00_2ndhash =
        "183C21016A80ED07A24C9E4BD299F8C04949F9D0B0566DF010211D57E6551164";
    String maskedExpect10_2ndhash =
        "0582EC7B89C8E0E4D50A59B97EAED03AE2DFA55946FA84C09058C6C743235D49";

    "TestString.x.x".length();

    // TEST A: Mask one index differently

    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/extension[*]/extension[0]/valueString", hashRuleName);
    groupMaskConf.put("/extension[*]/extension[*]/valueString", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));


    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);
    JsonConfig jsonConfig = testMaskingConfig.getJson();
    jsonConfig.addMaskingRule("/identifier/value", hashRuleName);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValue = genericMaskingProvider.mask(group);

    // Everything was hashed
    assertNotEquals(maskedExpect00,
        maskedValue.get("extension").get(0).get("extension").get(0).get("valueString").asText());
    assertNotEquals(maskedExpect10,
        maskedValue.get("extension").get(1).get("extension").get(0).get("valueString").asText());
    assertEquals(maskedExpect00_2ndhash,
        maskedValue.get("extension").get(0).get("extension").get(0).get("valueString").asText());
    assertEquals(maskedExpect10_2ndhash,
        maskedValue.get("extension").get(1).get("extension").get(0).get("valueString").asText());
    assertEquals(maskedExpect01,
        maskedValue.get("extension").get(0).get("extension").get(1).get("valueString").asText());
    assertEquals(maskedExpect11,
        maskedValue.get("extension").get(1).get("extension").get(1).get("valueString").asText());

    // Test the other values are unchanged
    assertEquals("TestURL.0.0",
        maskedValue.get("extension").get(0).get("extension").get(0).get("url").asText());
    assertEquals("TestURL.0.1",
        maskedValue.get("extension").get(0).get("extension").get(1).get("url").asText());
    assertEquals("TestURL.1.0",
        maskedValue.get("extension").get(1).get("extension").get(0).get("url").asText());
    assertEquals("TestURL.1.1",
        maskedValue.get("extension").get(1).get("extension").get(1).get("url").asText());
  }

  // The purpose of this test is to check output is not "accumulated"
  @Test
  public void testNestedArrayMask_MultipleRules() throws Exception {
    // Expected Values
    String maskedExpect00 = "10EFDE3500F99326821F384D2FD878D019B9C7E658001B07F8F491D63697C820";
    String maskedExpect01 = "9DB7D558482AF53B524217A5F4A69DE1C6F78A213782F1A41C20711B3155E4E1";
    String maskedExpect10 = "0DD3C918ADB16575C47F4D8B27862D86EE67BD5EFB2BAFDA2AEBF661254A21A1";
    String maskedExpect11 = "88CAFB31A719642B7DA0ADB63E72C015B4DCC66C4247D4EAC8CED064A8360C06";

    int originalLength = "TestString.x.x".length();

    // TEST A: Mask one index differently

    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/extension[*]/extension[0]/valueString", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValue = genericMaskingProvider.mask(group);

    assertEquals(maskedExpect00,
        maskedValue.get("extension").get(0).get("extension").get(0).get("valueString").asText());
    assertNotEquals(maskedExpect01,
        maskedValue.get("extension").get(0).get("extension").get(1).get("valueString").asText());
    assertEquals(maskedExpect10,
        maskedValue.get("extension").get(1).get("extension").get(0).get("valueString").asText());
    assertNotEquals(maskedExpect11,
        maskedValue.get("extension").get(1).get("extension").get(1).get("valueString").asText());

    assertEquals("TestString.0.1",
        maskedValue.get("extension").get(0).get("extension").get(1).get("valueString").asText());
    assertEquals("TestString.1.1",
        maskedValue.get("extension").get(1).get("extension").get(1).get("valueString").asText());

    // Test the other values are unchanged
    assertEquals("TestURL.0.0",
        maskedValue.get("extension").get(0).get("extension").get(0).get("url").asText());
    assertEquals("TestURL.0.1",
        maskedValue.get("extension").get(0).get("extension").get(1).get("url").asText());
    assertEquals("TestURL.1.0",
        maskedValue.get("extension").get(1).get("extension").get(0).get("url").asText());
    assertEquals("TestURL.1.1",
        maskedValue.get("extension").get(1).get("extension").get(1).get("url").asText());

    // Test B: Now mask the rest of the array

    Map<String, String> groupMaskConf2 = new HashMap<>();
    groupMaskConf2.put("/fhir/Group/extension[*]/extension[1,*]/valueString", randomRuleName);
    FHIRResourceMaskingConfiguration resourceConfiguration2 =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf2);

    MaskingProviderBuilder genericMaskingProvider2 =
        new MaskingProviderBuilder(schemaType, resourceConfiguration2, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValue2 = genericMaskingProvider2.mask(group);

    // Check unaffected values from before (extension[*]/extension[0])
    assertEquals(maskedExpect00,
        maskedValue2.get("extension").get(0).get("extension").get(0).get("valueString").asText());
    assertEquals(maskedExpect10,
        maskedValue2.get("extension").get(1).get("extension").get(0).get("valueString").asText());

    // Check that the rest of the values are RANDOMized (not hashed & not
    // the same)
    assertNotEquals(maskedExpect01,
        maskedValue2.get("extension").get(0).get("extension").get(1).get("valueString").asText());
    assertNotEquals(maskedExpect11,
        maskedValue2.get("extension").get(1).get("extension").get(1).get("valueString").asText());
    assertNotEquals("TestString.0.1",
        maskedValue2.get("extension").get(0).get("extension").get(1).get("valueString").asText());
    assertNotEquals("TestString.1.1",
        maskedValue2.get("extension").get(1).get("extension").get(1).get("valueString").asText());

    // Can tell if RANDOM if still the same length but not the same value
    // (tested above)
    assertEquals(originalLength, maskedValue2.get("extension").get(0).get("extension").get(1)
        .get("valueString").asText().length());
    assertEquals(originalLength, maskedValue2.get("extension").get(1).get("extension").get(1)
        .get("valueString").asText().length());
  }

  // Test valid array index ranges
  @Test
  public void testArrayMaskHash_RangeIndices_Valid() throws Exception {
    // Expected values
    String maskedExpect0 = "74F3E37238EEEB6E9FFC731106D53C689FED464D1BDF3633C9A247D942DDD8D9";
    String maskedExpect1 = "38DBA034F94A84CD77EBFD04AC4B11DAAC185207DC9DCC313B91B9172796ED3F";
    String maskedExpect2 = "5179C2D2C2995F87598ACF637439A179914637F5CCDCC3561C5D1A0743A8061A";

    // TEST D: Test index [0,2] with range (same as [*])

    JsonNode groupD = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfD = new HashMap<>();
    groupMaskConfD.put("/fhir/Group/identifier[0,2]/value", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationD =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfD);

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder maskingProviderD =
        new MaskingProviderBuilder(schemaType, resourceConfigurationD, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValueD = maskingProviderD.mask(groupD);
    assertEquals(maskedExpect0, maskedValueD.get("identifier").get(0).get("value").asText());
    assertEquals(maskedExpect1, maskedValueD.get("identifier").get(1).get("value").asText());
    assertEquals(maskedExpect2, maskedValueD.get("identifier").get(2).get("value").asText());

    // TEST E: Test index [1,2] with range (only mask identifier[1] & 2

    JsonNode groupE = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfE = new HashMap<>();
    groupMaskConfE.put("/fhir/Group/identifier[1,2]/value", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationE =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfE);

    MaskingProviderBuilder maskingProviderE =
        new MaskingProviderBuilder(schemaType, resourceConfigurationE, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);
    JsonNode maskedValueE = maskingProviderE.mask(groupE);
    assertEquals("ID0", maskedValueE.get("identifier").get(0).get("value").asText());
    assertEquals(maskedExpect1, maskedValueE.get("identifier").get(1).get("value").asText());
    assertEquals(maskedExpect2, maskedValueE.get("identifier").get(2).get("value").asText());

    // TEST F: Test index [1,*] with empty max index (should mask indices 1
    // & 2)

    JsonNode groupF = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfF = new HashMap<>();
    groupMaskConfF.put("/fhir/Group/identifier[1,*]/value", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationF =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfF);

    MaskingProviderBuilder maskingProviderF =
        new MaskingProviderBuilder(schemaType, resourceConfigurationF, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValueF = maskingProviderF.mask(groupF);
    assertEquals("ID0", maskedValueF.get("identifier").get(0).get("value").asText());
    assertEquals(maskedExpect1, maskedValueF.get("identifier").get(1).get("value").asText());
    assertEquals(maskedExpect2, maskedValueF.get("identifier").get(2).get("value").asText());

    // TEST G: Test index [0,0] end at 0 only masks first element

    JsonNode groupG = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfG = new HashMap<>();
    groupMaskConfG.put("/fhir/Group/identifier[0,0]/value", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationG =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfG);

    MaskingProviderBuilder maskingProviderG =
        new MaskingProviderBuilder(schemaType, resourceConfigurationG, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValueG = maskingProviderG.mask(groupG);
    assertEquals(maskedExpect0, maskedValueG.get("identifier").get(0).get("value").asText());
    assertEquals("ID1", maskedValueG.get("identifier").get(1).get("value").asText());
    assertEquals("ID2", maskedValueG.get("identifier").get(2).get("value").asText());
    assertEquals("ID1", maskedValueG.get("identifier").get(1).get("value").asText());
    assertEquals("ID2", maskedValueG.get("identifier").get(2).get("value").asText());
  }

  //
  // Test invalid array index ranges
  @Test
  public void testArrayMaskHash_RangeIndices_Invalid() throws Exception {
    // Expected values
    String maskedExpect0 = "74F3E37238EEEB6E9FFC731106D53C689FED464D1BDF3633C9A247D942DDD8D9";
    String maskedExpect1 = "38DBA034F94A84CD77EBFD04AC4B11DAAC185207DC9DCC313B91B9172796ED3F";
    String maskedExpect2 = "5179C2D2C2995F87598ACF637439A179914637F5CCDCC3561C5D1A0743A8061A";

    // TEST A

    JsonNode groupA = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfA = new HashMap<>();
    groupMaskConfA.put("/fhir/Group", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationA =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfA);

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder maskingProviderA =
        new MaskingProviderBuilder(schemaType, resourceConfigurationA, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValueA = maskingProviderA.mask(groupA);
    assertEquals("ID0", maskedValueA.get("identifier").get(0).get("value").asText());
    assertEquals("ID1", maskedValueA.get("identifier").get(1).get("value").asText());
    assertEquals("ID2", maskedValueA.get("identifier").get(2).get("value").asText());

    // TEST B

    JsonNode groupB = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfB = new HashMap<>();
    groupMaskConfB.put("/fhir/Group", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationB =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfB);

    MaskingProviderBuilder maskingProviderB =
        new MaskingProviderBuilder(schemaType, resourceConfigurationB, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    maskingProviderB.mask(groupB);
    assertEquals("ID0", maskedValueA.get("identifier").get(0).get("value").asText());
    assertEquals("ID1", maskedValueA.get("identifier").get(1).get("value").asText());
    assertEquals("ID2", maskedValueA.get("identifier").get(2).get("value").asText());

    // TEST C

    JsonNode groupC = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfC = new HashMap<>();
    groupMaskConfC.put("/fhir/Group/identifier[,,,,n]/value", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationC =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfC);
    MaskingProviderBuilder maskingProviderC =
        new MaskingProviderBuilder(schemaType, resourceConfigurationC, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);
    maskingProviderC.mask(groupC);
    assertEquals("ID0", maskedValueA.get("identifier").get(0).get("value").asText());
    assertEquals("ID1", maskedValueA.get("identifier").get(1).get("value").asText());
    assertEquals("ID2", maskedValueA.get("identifier").get(2).get("value").asText());

    // TEST D: Test invalid max index ranges e.g. [n,,5] (outside boundary
    // of 2)

    JsonNode groupD = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfD = new HashMap<>();
    groupMaskConfD.put("/fhir/Group/identifier[0,5]/value", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationD =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfD);
    MaskingProviderBuilder maskingProviderD =
        new MaskingProviderBuilder(schemaType, resourceConfigurationD, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);
    JsonNode maskedValueD = maskingProviderD.mask(groupD);
    assertEquals(maskedExpect0, maskedValueD.get("identifier").get(0).get("value").asText());
    assertEquals(maskedExpect1, maskedValueD.get("identifier").get(1).get("value").asText());
    assertEquals(maskedExpect2, maskedValueD.get("identifier").get(2).get("value").asText());

    // TEST E: Test invalid negative indices (not accepted in the masking)

    JsonNode groupE = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfE = new HashMap<>();
    groupMaskConfE.put("/fhir/Group", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationE =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfE);

    MaskingProviderBuilder maskingProviderE =
        new MaskingProviderBuilder(schemaType, resourceConfigurationE, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);
    JsonNode maskedValueE = maskingProviderE.mask(groupE);
    assertEquals("ID0", maskedValueE.get("identifier").get(0).get("value").asText());
    assertEquals("ID1", maskedValueE.get("identifier").get(1).get("value").asText());
    assertEquals("ID2", maskedValueE.get("identifier").get(2).get("value").asText());

    // TEST F: Test index [*,1] with * min index - not supported

    JsonNode groupF = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfF = new HashMap<>();
    groupMaskConfF.put("/fhir/Group/identifier[*,1]/value", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationF =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfF);

    MaskingProviderBuilder maskingProviderF =
        new MaskingProviderBuilder(schemaType, resourceConfigurationF, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);
    JsonNode maskedValueF = maskingProviderF.mask(groupF);
    assertEquals("ID0", maskedValueF.get("identifier").get(0).get("value").asText());
    assertEquals("ID1", maskedValueF.get("identifier").get(1).get("value").asText());
    assertEquals("ID2", maskedValueF.get("identifier").get(2).get("value").asText());
  }

  // Test invalid array index ranges (old format using n)
  @Test
  public void testArrayMaskHash_RangeIndices_Invalid2() throws Exception {
    // TEST A: Test index [,] (not supported)

    JsonNode groupA = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfA = new HashMap<>();
    groupMaskConfA.put("/fhir/Group/identifier[,]/value", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationA =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfA);
    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder maskingProviderA =
        new MaskingProviderBuilder(schemaType, resourceConfigurationA, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValueA = maskingProviderA.mask(groupA);
    assertEquals("ID0", maskedValueA.get("identifier").get(0).get("value").asText());
    assertEquals("ID1", maskedValueA.get("identifier").get(1).get("value").asText());
    assertEquals("ID2", maskedValueA.get("identifier").get(2).get("value").asText());

    // TEST B: Test index [n,]

    JsonNode groupB = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfB = new HashMap<>();
    groupMaskConfB.put("/fhir/Group", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationB =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfB);
    MaskingProviderBuilder maskingProviderB =
        new MaskingProviderBuilder(schemaType, resourceConfigurationB, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    maskingProviderB.mask(groupB);
    assertEquals("ID0", maskedValueA.get("identifier").get(0).get("value").asText());
    assertEquals("ID1", maskedValueA.get("identifier").get(1).get("value").asText());
    assertEquals("ID2", maskedValueA.get("identifier").get(2).get("value").asText());

    // TEST C: Test index n

    JsonNode groupC = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfC = new HashMap<>();
    groupMaskConfC.put("/fhir/Group/identifier[n]/value", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationC =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfC);
    MaskingProviderBuilder maskingProviderC =
        new MaskingProviderBuilder(schemaType, resourceConfigurationC, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    maskingProviderC.mask(groupC);
    assertEquals("ID0", maskedValueA.get("identifier").get(0).get("value").asText());
    assertEquals("ID1", maskedValueA.get("identifier").get(1).get("value").asText());
    assertEquals("ID2", maskedValueA.get("identifier").get(2).get("value").asText());

    // TEST D: Test with 3 ranges - invalid format, will be ignored and no
    // masking will be done

    JsonNode groupD = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfD = new HashMap<>();
    groupMaskConfD.put("/fhir/Group", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationD =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfD);
    MaskingProviderBuilder maskingProviderD =
        new MaskingProviderBuilder(schemaType, resourceConfigurationD, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);
    maskingProviderD.mask(groupD);
    assertEquals("ID0", maskedValueA.get("identifier").get(0).get("value").asText());
    assertEquals("ID1", maskedValueA.get("identifier").get(1).get("value").asText());
    assertEquals("ID2", maskedValueA.get("identifier").get(2).get("value").asText());
  }

  @Test
  public void testArrayMaskHash_RangeIndices_Spaces() throws Exception {

    // Expected values
    String maskedExpect1 = "38DBA034F94A84CD77EBFD04AC4B11DAAC185207DC9DCC313B91B9172796ED3F";
    String maskedExpect2 = "5179C2D2C2995F87598ACF637439A179914637F5CCDCC3561C5D1A0743A8061A";

    JsonNode groupC = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfC = new HashMap<>();
    groupMaskConfC.put("/fhir/Group/identifier[1, 2]/value", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationC =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfC);
    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder maskingProviderC =
        new MaskingProviderBuilder(schemaType, resourceConfigurationC, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValueC = maskingProviderC.mask(groupC);
    assertEquals("ID0", maskedValueC.get("identifier").get(0).get("value").asText());
    assertEquals(maskedExpect1, maskedValueC.get("identifier").get(1).get("value").asText());
    assertEquals(maskedExpect2, maskedValueC.get("identifier").get(2).get("value").asText());
  }

  @Test
  public void testArrayMaskHash_ListIndices() throws Exception {
    // Expected values
    String maskedExpect0 = "74F3E37238EEEB6E9FFC731106D53C689FED464D1BDF3633C9A247D942DDD8D9";
    String maskedExpect1 = "38DBA034F94A84CD77EBFD04AC4B11DAAC185207DC9DCC313B91B9172796ED3F";
    String maskedExpect2 = "5179C2D2C2995F87598ACF637439A179914637F5CCDCC3561C5D1A0743A8061A";

    // TEST A: Test index 0,2

    JsonNode groupA = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfA = new HashMap<>();
    groupMaskConfA.put("/fhir/Group/identifier[{0,2}]/value", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationA =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfA);

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder maskingProviderA =
        new MaskingProviderBuilder(schemaType, resourceConfigurationA, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    JsonNode maskedValueA = maskingProviderA.mask(groupA);
    assertEquals(maskedExpect0, maskedValueA.get("identifier").get(0).get("value").asText());
    assertEquals("ID1", maskedValueA.get("identifier").get(1).get("value").asText());
    assertEquals(maskedExpect2, maskedValueA.get("identifier").get(2).get("value").asText());

    // TEST B: test single index 0 (same as [0])

    JsonNode groupB = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfB = new HashMap<>();
    groupMaskConfB.put("/fhir/Group/identifier[{0}]/value", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationB =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfB);
    MaskingProviderBuilder maskingProviderB =
        new MaskingProviderBuilder(schemaType, resourceConfigurationB, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);
    JsonNode maskedValueB = maskingProviderB.mask(groupB);
    assertEquals(maskedExpect0, maskedValueB.get("identifier").get(0).get("value").asText());
    assertEquals("ID1", maskedValueB.get("identifier").get(1).get("value").asText());
    assertEquals("ID2", maskedValueB.get("identifier").get(2).get("value").asText());

    // TEST C: test index 0 with extra comma - should not choke on it

    JsonNode groupC = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfC = new HashMap<>();
    groupMaskConfC.put("/fhir/Group/identifier[{0,}]/value", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationC =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfC);
    MaskingProviderBuilder maskingProviderC =
        new MaskingProviderBuilder(schemaType, resourceConfigurationC, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);
    JsonNode maskedValueC = maskingProviderC.mask(groupC);
    assertEquals(maskedExpect0, maskedValueC.get("identifier").get(0).get("value").asText());
    assertEquals("ID1", maskedValueC.get("identifier").get(1).get("value").asText());
    assertEquals("ID2", maskedValueC.get("identifier").get(2).get("value").asText());

    // TEST D: Test forgetting curly braces, would be considered a range of
    // [start_index,end_index]

    JsonNode groupD = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfD = new HashMap<>();
    groupMaskConfD.put("/fhir/Group/identifier[0,2]/value", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationD =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfD);
    MaskingProviderBuilder maskingProviderD =
        new MaskingProviderBuilder(schemaType, resourceConfigurationD, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);
    JsonNode maskedValueD = maskingProviderD.mask(groupD);
    assertEquals(maskedExpect0, maskedValueD.get("identifier").get(0).get("value").asText());
    assertEquals(maskedExpect1, maskedValueD.get("identifier").get(1).get("value").asText());

    assertEquals(maskedExpect2, maskedValueD.get("identifier").get(2).get("value").asText());
  }

  @Test
  public void testArrayMaskHash_ListIndices_Spaces() throws Exception {
    // Expected values
    String maskedExpect0 = "74F3E37238EEEB6E9FFC731106D53C689FED464D1BDF3633C9A247D942DDD8D9";
    String maskedExpect2 = "5179C2D2C2995F87598ACF637439A179914637F5CCDCC3561C5D1A0743A8061A";

    // TEST A: Test index 0,2

    JsonNode groupA = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));
    Map<String, String> groupMaskConfA = new HashMap<>();
    groupMaskConfA.put("/fhir/Group/identifier[ {0 , 2} ]/value", hashRuleName);
    FHIRResourceMaskingConfiguration resourceConfigurationA =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConfA);

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder maskingProviderA =
        new MaskingProviderBuilder(schemaType, resourceConfigurationA, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);
    JsonNode maskedValueA = maskingProviderA.mask(groupA);
    assertEquals(maskedExpect0, maskedValueA.get("identifier").get(0).get("value").asText());
    assertEquals("ID1", maskedValueA.get("identifier").get(1).get("value").asText());
    assertEquals(maskedExpect2, maskedValueA.get("identifier").get(2).get("value").asText());
  }

  @Test
  public void testArrayMaskHash_Empty() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/fhir/Group/identifier[]/value", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);
    // Check that the items were not masked
    JsonNode maskedValue = genericMaskingProvider.mask(group);
    assertEquals("ID0", maskedValue.get("identifier").get(0).get("system").asText());
    assertEquals("ID0", maskedValue.get("identifier").get(0).get("value").asText());
    assertEquals("ID1", maskedValue.get("identifier").get(1).get("system").asText());
    assertEquals("ID1", maskedValue.get("identifier").get(1).get("value").asText());
    assertEquals("ID2", maskedValue.get("identifier").get(2).get("system").asText());
    assertEquals("ID2", maskedValue.get("identifier").get(2).get("value").asText());
  }

  @Test
  public void testArrayMaskHash_NoElementDefined() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/characteristic[1]", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    // Check that the items were not masked
    JsonNode originalCharacteristic = group.get("characteristic");
    JsonNode maskedValue = genericMaskingProvider.mask(group);
    JsonNode maskedCharacteristic = maskedValue.get("characteristic");
    assertEquals(originalCharacteristic.toString(), maskedCharacteristic.toString());
  }

  @Test
  public void testArrayMaskHash_NonArray() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/name[1]", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    // Check that the items were not masked
    JsonNode originalName = group.get("name");
    JsonNode maskedValue = genericMaskingProvider.mask(group);
    JsonNode maskedName = maskedValue.get("name");
    assertEquals(originalName.toString(), maskedName.toString());
  }

  @Test
  public void testArrayIndexWithArrayQuery_Valid() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/extension/extension/url(valueString==TestString.1.0)", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
    testMaskingConfig = addRules(testMaskingConfig);

    MaskingProviderBuilder genericMaskingProvider =
        new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
            arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);

    // Check that the items was masked
    JsonNode maskedValue = genericMaskingProvider.mask(group);
    String expectedURL = "B04B581A8CB5EF48D637B89D232228F2598A456D91B187DD723180D9B6E7DA1E";
    assertEquals(expectedURL,
        maskedValue.get("extension").get(1).get("extension").get(0).get("url").asText());
  }

  @Test
  public void testArrayIndexWithArrayQuery_InValid() throws Exception {
    Map<String, String> groupMaskConf = new HashMap<>();
    groupMaskConf.put("/extension[1]/extension/url(valueString==TestString.1.0)", hashRuleName);

    FHIRResourceMaskingConfiguration resourceConfiguration =
        new FHIRResourceMaskingConfiguration("/fhir/Group", groupMaskConf);

    JsonNode group = new ObjectMapper()
        .readTree(this.getClass().getResourceAsStream("/fhir/examples/group-example-array.json"));

    boolean isSuccessful = false;
    try {

      DeidMaskingConfig testMaskingConfig = (new ConfigGenerator()).getTestDeidConfig();
      testMaskingConfig = addRules(testMaskingConfig);

      MaskingProviderBuilder maskingProvider =
          new MaskingProviderBuilder(schemaType, resourceConfiguration, testMaskingConfig,
              arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId);
      maskingProvider.mask(group);
      isSuccessful = true;
    } catch (IllegalArgumentException e) {
      String expectedText =
          "Cannot intermix fhir arrays by index and arrays by query in the same masking rule";
      assertEquals(expectedText, e.getMessage());
    }
    // Make sure it didn't actually succeed
    assertEquals(false, isSuccessful);
  }


}
