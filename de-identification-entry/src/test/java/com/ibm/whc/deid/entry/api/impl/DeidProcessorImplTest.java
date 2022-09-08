/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.entry.api.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.entry.api.DeidEntry;
import com.ibm.whc.deid.entry.api.DeidProcessor;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.Rule;
import com.ibm.whc.deid.shared.pojo.config.json.JsonMaskingRule;
import com.ibm.whc.deid.shared.pojo.config.masking.AddressMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.CityMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.ContinentMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.HashMaskingProviderConfig;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class DeidProcessorImplTest {

  private ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
  private String goodConfigString = null;

  @Before
  public void setup() throws Exception {
    goodConfigString = new String(Files.readAllBytes(Paths
        .get(getClass().getResource("/DeidProcessorImplTest_files/simple.config.json").toURI())));
  }

  @Test
  public void testHappyPath() throws Exception {
    List<String> data = Files.readAllLines(
        Paths.get(getClass().getResource("/DeidProcessorImplTest_files/binning.data.txt").toURI()));
    String config = new String(Files.readAllBytes(Paths
        .get(getClass().getResource("/DeidProcessorImplTest_files/binning.config.json").toURI())));
    
    DeidProcessor proc = DeidEntry.getDeidProcessor(config);
    assertNotNull(proc);

    List<String> out = proc.process(data);
    assertNotNull(out);
    assertEquals(3, out.size());
    assertEquals("{\"a\":4,\"b\":\"b\",\"c\":{\"a\":5,\"b\":\"5-10\",\"k\":\"1000-1005\"}}",
        out.get(0));
    assertEquals("{\"a\":4,\"b\":\"b\",\"c\":{\"a\":23,\"b\":\"20-25\",\"k\":\"1020-1025\"}}",
        out.get(1));
    assertEquals("{\"a\":4,\"b\":\"b\",\"c\":{\"a\":117,\"b\":\"115-120\",\"k\":\"1115-1120\"}}",
        out.get(2));
  }

  @Test
  public void testMaskNullDataListMember() throws Exception {
    List<String> inputList = new ArrayList<>();
    inputList.add(null);
    DeidProcessor proc = DeidEntry.getDeidProcessor(goodConfigString);
    try {
      proc.process(inputList);
      fail("expected exception");
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage(), e.getMessage().contains("data[0]"));
    }
  }

  @Test
  public void testMaskNullData() throws Exception {
    List<String> inputList = null;
    DeidProcessor proc = DeidEntry.getDeidProcessor(goodConfigString);
    try {
      proc.process(inputList);
      fail("expected exception");
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage(), e.getMessage().endsWith(" data"));
    }

    inputList = Collections.emptyList();
    try {
      proc.process(inputList);
      fail("expected exception");
    } catch (IllegalArgumentException e) {
      assertTrue(e.getMessage(), e.getMessage().endsWith(" data"));
    }
  }

  @Test
  public void testMaskNullConfig() throws Exception {
    String config = null;
    try {
      DeidEntry.getDeidProcessor(config);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage().equals("no masking configuration data"));
    }
  }

  @Test
  public void testConfigNoJsonSection() throws Exception {
    DeidMaskingConfig configObj = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    configObj.setJson(null);
    String config = mapper.writeValueAsString(configObj);
    try {
      DeidEntry.getDeidProcessor(config);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage()
          .contains("Invalid masking configuration: the value of the `json` property is missing"));
    }
  }

  @Test
  public void testConfigJsonNoSchemaType() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    config.getJson().setSchemaType(null);
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage().contains("`json.schemaType` property is missing"));
    }
  }

  @Test
  public void testConfigNullMessageTypes() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    config.getJson().setMessageTypes(null);
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage()
          .contains("`json.messageTypes` must be provided when `json.messageTypeKey` is provided"));
    }
  }

  @Test
  public void testConfigEmptyMessageTypes() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    config.getJson().setMessageTypes(new ArrayList<>());
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage()
          .contains("`json.messageTypes` must be provided when `json.messageTypeKey` is provided"));
    }
  }

  @Test
  public void testConfigEmptyInMessageType() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    config.getJson().getMessageTypes().add(0, "");
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(),
          e.getMessage().contains("value at offset 0 in `json.messageTypes` is missing"));
    }
  }

  @Test
  public void testConfigNullInMessageType() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    config.getJson().getMessageTypes().add(1, null);
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(),
          e.getMessage().contains("value at offset 1 in `json.messageTypes` is missing"));
    }
  }

  @Test
  public void testConfigWhitespaceInMessageType() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    config.getJson().getMessageTypes().add(2, "  \t");
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(),
          e.getMessage().contains("value at offset 2 in `json.messageTypes` is missing"));
    }
  }

  @Test
  public void testConfigJsonNullRule() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    config.getJson().getMaskingRules().add(0, null);
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage().contains(
          "`rule` property is missing from the rule assignment at offset 0 in `json.maskingRules`"));
    }
  }

  @Test
  public void testConfigJsonRuleNameNull() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    config.getJson().getMaskingRules().add(1, new JsonMaskingRule("/fhir/patient/number", null));
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage().contains(
          "`rule` property is missing from the rule assignment at offset 1 in `json.maskingRules`"));
    }
  }

  @Test
  public void testConfigJsonPathNull() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    config.getJson().getMaskingRules().add(2, new JsonMaskingRule(null, "HASH"));
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage().contains(
          "`jsonPath` property in the rule assignment at offset 2 in `json.maskingRules` must start with `/`"));
    }
  }

  @Test
  public void testConfigJsonPathBad() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    int offset = config.getJson().getMaskingRules().size();
    config.getJson().getMaskingRules().add(new JsonMaskingRule("", "HASH"));
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(),
          e.getMessage().contains("`jsonPath` property in the rule assignment at offset " + offset
              + " in `json.maskingRules` must start with `/`"));
    }
  }

  @Test
  public void testConfigJsonPathDuplicate() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    List<JsonMaskingRule> assignments = config.getJson().getMaskingRules();
    assignments.add(3, new JsonMaskingRule("/fhir/test/null1", "HASH"));
    assignments.add(5, new JsonMaskingRule("/fhir/test/path2", "HASH"));
    // complete duplicate - ignored
    assignments.add(7, new JsonMaskingRule("/fhir/test/path1", "HASH"));
    assignments.add(7, new JsonMaskingRule("/fhir/test/path2", "CONTINENT"));
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage()
          .contains(
              "Invalid masking configuration: the path `/fhir/test/path2` is assigned to the `CONTINENT` rule but that path is already assigned to the `HASH` rule"));
    }
  }

  @Test
  public void testConfigRuleAssignmentMismatch() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    config.getJson().getMaskingRules().get(0).setRule("no_1");
    config.getJson().getMaskingRules().get(1).setRule("");
    config.getJson().getMaskingRules().get(2).setRule("  ");
    config.getJson().getMaskingRules().add(new JsonMaskingRule("/fhir/bad/rule", "no_last"));
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage().contains(
          "Invalid masking configuration: the rule assignment with `rule` value `no_1` does not refer to a valid rule.  There are 4 such invalid rules."));
    }
  }

  @Test
  public void testConfigNullRule() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    int count = config.getRules().size();
    config.getRules().add(null);
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage().contains(
          "Invalid masking configuration: the rule at offset " + count + " in `rules` is null"));
    }
  }

  @Test
  public void testConfigNullRuleName() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    config.getRules().add(1, new Rule(null, Arrays.asList(new HashMaskingProviderConfig())));
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage().contains(
          "Invalid masking configuration: the `name` property is missing from the rule at offset 1 in `rules`"));
    }
  }

  @Test
  public void testConfigEmptyRuleName() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    config.getRules().add(2, new Rule("  \t \n", Arrays.asList(new HashMaskingProviderConfig())));
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage().contains(
          "Invalid masking configuration: the `name` property is missing from the rule at offset 2 in `rules`"));
    }
  }

  @Test
  public void testConfigDuplicateRuleName() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    String ruleName = config.getRules().get(0).getName();
    config.getRules().add(new Rule(ruleName, Arrays.asList(new HashMaskingProviderConfig())));
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(),
          e.getMessage().contains("Invalid masking configuration: the value `" + ruleName
              + "` is used for the `name` property on multiple rules in the `rules` list - rule names must be unique"));
    }
  }

  @Test
  public void testConfigRuleNoMaskingProviders() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    config.getRules().get(2).setMaskingProviders(null);
    String ruleName = config.getRules().get(2).getName();
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage().contains(
          "Invalid masking configuration: the `maskingProviders` property is missing from the rule with `name` value `"
              + ruleName + "` in `rules`"));
    }
  }

  @Test
  public void testConfigNullMaskingProvider() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    config.getRules().get(0).getMaskingProviders().add(null);
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage().contains(
          "Invalid masking configuration: the second masking provider in `maskingProviders` for the rule with `name` value `HASH` in `rules` is null"));
    }
  }

  @Test
  public void testConfigTooManyProviders() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    String ruleName = config.getRules().get(1).getName();
    config.getRules().get(1).getMaskingProviders().add(new HashMaskingProviderConfig());
    config.getRules().get(1).getMaskingProviders().add(new HashMaskingProviderConfig());
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage().contains(
          "Invalid masking configuration: too many entries in `maskingProviders` for the rule with `name` value `"
              + ruleName + "` in `rules` - the maximum allowed is 2"));
    }
  }

  @Test
  public void testConfigCategory2First() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    config.getRules().get(2).getMaskingProviders().add(0, new HashMaskingProviderConfig());
    String ruleName = config.getRules().get(2).getName();
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(),
          e.getMessage().contains("Invalid masking configuration: the rule with `name` value `"
              + ruleName
              + "` in `rules` contains multiple masking providers, but the first masking provider is not a Category I provider"));
    }
  }

  @Test
  public void testConfigCategory1Second() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    config.getRules().add(1, new Rule("multiRuleX",
        Arrays.asList(new ContinentMaskingProviderConfig(), new CityMaskingProviderConfig())));
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage().contains(
          "Invalid masking configuration: the rule with `name` value `multiRuleX` in `rules` contains multiple masking providers, but the second masking provider is not a Category II provider"));
    }
  }

  @Test
  public void testConfigInvalidProviderConfig() throws Exception {
    DeidMaskingConfig config = mapper.readValue(goodConfigString, DeidMaskingConfig.class);
    AddressMaskingProviderConfig provider = new AddressMaskingProviderConfig();
    provider.setPostalCodeNearestK(-1);
    config.getRules().add(1, new Rule("invalidRuleX", Arrays.asList(provider)));
    String configString = mapper.writeValueAsString(config);
    try {
      DeidEntry.getDeidProcessor(configString);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertTrue(e.getMessage(), e.getMessage().contains(
          "Invalid masking configuration: the first masking provider in `maskingProviders` for the rule with `name` value `invalidRuleX` in `rules` is not valid: `postalCodeNearestK` must be greater than 0"));
    }
  }
}
