/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint.datamasking;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultHandlers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.app.endpoint.Application;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.Rule;
import com.ibm.whc.deid.shared.pojo.config.json.JsonMaskingRule;
import com.ibm.whc.deid.shared.pojo.config.masking.AddressMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.CityMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.ContinentMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.GenderMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.HashMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.masking.DataMaskingModel;

@RunWith(SpringRunner.class)
// force using a test profile to avoid using any other active profile
// we do not have a real application-test.properties.
@ActiveProfiles(profiles = {"test"})
@AutoConfigureMockMvc
@SpringBootTest(classes = Application.class)
public class DataMaskingControllerErrorPathTest {

  private final String basePath = "/api/v1";

  private MockMvc mockMvc;

  private static final Logger log =
      LoggerFactory.getLogger(DataMaskingControllerErrorPathTest.class);

  private static String TEST_CONFIG;
  private static String TEST_DATA;

  @Autowired
  private WebApplicationContext wac;

  @BeforeClass
  public static void testSetup() throws Exception {
    TEST_CONFIG = new String(Files.readAllBytes(Paths.get(DataMaskingControllerErrorPathTest.class
        .getResource("/config/fhir/masking_config.json").toURI())));
    TEST_DATA = new String(Files.readAllBytes(Paths.get(DataMaskingControllerErrorPathTest.class
        .getResource("/masking/data/simple_fhir.json").toURI())));
  }

  @Before
  public void setup() {
    this.mockMvc = MockMvcBuilders.webAppContextSetup(wac).build();
  }

  @Test
  public void testMaskEmptyInput() throws Exception {
    String noContent = "";
    String emptyObject = "{}";
    log.info(noContent);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(noContent))
        .andDo(print()).andExpect(status().isBadRequest()).andDo(MockMvcResultHandlers.print())
        .andExpect(content().string(startsWith("Required request body is missing")));

    log.info(emptyObject);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(emptyObject))
        .andDo(print()).andExpect(status().isBadRequest()).andDo(MockMvcResultHandlers.print())
        .andExpect(content().string("no configuration data"));
  }

  @Test
  public void testMaskNullDataListMember() throws Exception {
    String data = null;

    List<String> inputList = new ArrayList<>();
    inputList.add(data);
    DataMaskingModel dataMaskingModel =
        new DataMaskingModel(TEST_CONFIG, inputList, ConfigSchemaType.FHIR);
    ObjectMapper mapper = new ObjectMapper();
    String request = mapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest()).andDo(MockMvcResultHandlers.print())
        .andExpect(content().string("Invalid input error data[0]"));
  }

  @Test
  public void testMaskNullOrEmptyData() throws Exception {
    List<String> inputList = null;
    DataMaskingModel dataMaskingModel =
        new DataMaskingModel(TEST_CONFIG, inputList, ConfigSchemaType.FHIR);
    ObjectMapper mapper = new ObjectMapper();
    String request = mapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest()).andDo(MockMvcResultHandlers.print())
        .andExpect(content().string("Invalid input error data"));

    inputList = new ArrayList<>();
    dataMaskingModel = new DataMaskingModel(TEST_CONFIG, inputList, ConfigSchemaType.FHIR);
    request = mapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest()).andDo(MockMvcResultHandlers.print())
        .andExpect(content().string("Invalid input error data"));
  }

  @Test
  public void testMaskNullConfig() throws Exception {
    String config = null;

    List<String> inputList = new ArrayList<>();
    inputList.add(TEST_DATA);
    DataMaskingModel dataMaskingModel =
        new DataMaskingModel(config, inputList, ConfigSchemaType.FHIR);
    ObjectMapper mapper = new ObjectMapper();
    String request = mapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest()).andDo(MockMvcResultHandlers.print())
        .andExpect(content().string("no configuration data"));
  }

  @Test
  public void testNullSchemaType() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    ObjectNode rootNode = mapper.createObjectNode();
    ArrayNode dataNode = rootNode.putArray("data");
    dataNode.add(TEST_DATA);
    rootNode.put("config", TEST_CONFIG);
    rootNode.put("schemaType", (String) null);
    String request = mapper.writeValueAsString(rootNode);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest()).andDo(MockMvcResultHandlers.print())
        .andExpect(content().string(startsWith("Invalid input error schemaType")));
  }

  @Test
  public void testInvalidSchemaType() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    ObjectNode rootNode = mapper.createObjectNode();
    ArrayNode dataNode = rootNode.putArray("data");
    dataNode.add(TEST_DATA);
    rootNode.put("config", TEST_CONFIG);
    rootNode.put("schemaType", "invalid");
    String request = mapper.writeValueAsString(rootNode);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest()).andDo(MockMvcResultHandlers.print())
        .andExpect(content().string(startsWith("JSON parse error")));
  }

  @Test
  public void testConfigNoJsonSection() throws Exception {
    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();

    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);
    config.setJson(null);
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);
    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    ObjectMapper mapper = new ObjectMapper();
    String request = mapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: the value of the `json` property is missing")));
  }

  @Test
  public void testConfigJsonNoSchemaType() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);
    config.getJson().setSchemaType(null);

    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andExpect(status().isBadRequest()).andDo(print())
        .andExpect(content().string(containsString("`json.schemaType` property is missing")));
  }

  @Test
  public void testConfigNullMessageTypes() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    config.getJson().setMessageTypes(null);
    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: `json.messageTypes` must be provided when `json.messageTypeKey` is provided")));
  }

  @Test
  public void testConfigEmptyMessageTypes() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    config.getJson().setMessageTypes(new ArrayList<>());
    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: `json.messageTypes` must be provided when `json.messageTypeKey` is provided")));
  }

  @Test
  public void testConfigEmptyInMessageType() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    config.getJson().getMessageTypes().add(0, "");
    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: value at offset 0 in `json.messageTypes` is missing")));
  }

  @Test
  public void testConfigNullInMessageType() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    config.getJson().getMessageTypes().add(1, null);
    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: value at offset 1 in `json.messageTypes` is missing")));
  }

  @Test
  public void testConfigWhitespaceInMessageType() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    config.getJson().getMessageTypes().add(2, "  \t");
    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: value at offset 2 in `json.messageTypes` is missing")));
  }

  @Test
  public void testConfigJsonNullRule() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    config.getJson().getMaskingRules().add(0, null);
    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: `rule` property is missing from the rule assignment at offset 0 in `json.maskingRules`")));
  }

  @Test
  public void testConfigJsonRuleNameNull() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    config.getJson().getMaskingRules().add(1, new JsonMaskingRule("/fhir/patient/number", null));
    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: `rule` property is missing from the rule assignment at offset 1 in `json.maskingRules`")));
  }

  @Test
  public void testConfigJsonPathNull() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    config.getJson().getMaskingRules().add(2, new JsonMaskingRule(null, "HASH"));
    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: `jsonPath` property in the rule assignment at offset 2 in `json.maskingRules` must start with `/`")));
  }

  @Test
  public void testConfigJsonPathBad() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    int offset = config.getJson().getMaskingRules().size();
    config.getJson().getMaskingRules().add(new JsonMaskingRule("", "HASH"));
    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: `jsonPath` property in the rule assignment at offset "
                + offset + " in `json.maskingRules` must start with `/`")));
  }

  @Test
  public void testConfigRuleAssignmentMismatch() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    config.getJson().getMaskingRules().get(0).setRule("no_1");
    config.getJson().getMaskingRules().get(1).setRule("");
    config.getJson().getMaskingRules().get(2).setRule("  ");
    config.getJson().getMaskingRules().add(new JsonMaskingRule("/fhir/bad/rule", "no_last"));
    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: the rule assignment with `rule` value `no_1` does not refer to a valid rule.  There are 4 such invalid rules.")));
  }

  @Test
  public void testConfigNullRule() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    int count = config.getRules().size();
    config.getRules().add(null);
    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: the rule at offset " + count + " in `rules` is null")));
  }

  @Test
  public void testConfigNullRuleName() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    config.getRules().add(1, new Rule(null, Arrays.asList(new HashMaskingProviderConfig())));

    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: the `name` property is missing from the rule at offset 1 in `rules`")));
  }

  @Test
  public void testConfigEmptyRuleName() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    config.getRules().add(2, new Rule("  \t \n", Arrays.asList(new HashMaskingProviderConfig())));

    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: the `name` property is missing from the rule at offset 2 in `rules`")));
  }

  @Test
  public void testConfigDuplicateRuleName() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    String ruleName = config.getRules().get(0).getName();
    config.getRules().add(new Rule(ruleName, Arrays.asList(new HashMaskingProviderConfig())));

    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString("Invalid masking configuration: the value `"
            + ruleName
            + "` is used for the `name` property on multiple rules in the `rules` list - rule names must be unique")));
  }

  @Test
  public void testConfigRuleNoMaskingProviders() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    config.getRules().get(2).setMaskingProviders(null);
    String ruleName = config.getRules().get(2).getName();

    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: the `maskingProviders` property is missing from the rule with `name` value `"
                + ruleName + "` in `rules`")));
  }

  @Test
  public void testConfigNullMaskingProvider() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    config.getRules().get(2).getMaskingProviders().add(null);
    String ruleName = config.getRules().get(2).getName();

    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: the second masking provider in `maskingProviders` for the rule with `name` value `"
                + ruleName + "` in `rules` is null")));

    config.getRules().get(2).getMaskingProviders().remove(1);
    config.getRules().get(2).getMaskingProviders().add(0, null);

    dataMaskingModel = new DataMaskingModel(objectMapper.writeValueAsString(config), dataList,
        ConfigSchemaType.FHIR);
    request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: the first masking provider in `maskingProviders` for the rule with `name` value `"
                + ruleName + "` in `rules` is null")));
  }

  @Test
  public void testConfigTooManyProviders() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);

    String ruleName = config.getRules().get(0).getName();
    config.getRules().get(0).getMaskingProviders().add(new HashMaskingProviderConfig());
    config.getRules().get(0).getMaskingProviders().add(new HashMaskingProviderConfig());

    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: too many entries in `maskingProviders` for the rule with `name` value `"
                + ruleName + "` in `rules` - the maximum allowed is 2")));
  }

  @Test
  public void testConfigCategory2First() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);
    config.getRules().get(0).getMaskingProviders().add(0, new HashMaskingProviderConfig());
    String ruleName = config.getRules().get(0).getName();

    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(
            containsString("Invalid masking configuration: the rule with `name` value `" + ruleName
                + "` in `rules` contains multiple masking providers, but the first masking provider is not a Category I provider")));
  }

  @Test
  public void testConfigCategory1Second() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);
    config.getRules().add(2, new Rule("multiRuleX",
        Arrays.asList(new ContinentMaskingProviderConfig(), new CityMaskingProviderConfig())));

    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: the rule with `name` value `multiRuleX` in `rules` contains multiple masking providers, but the second masking provider is not a Category II provider")));
  }

  @Test
  public void testConfigInvalidProviderConfig() throws Exception {
    List<String> dataList = new ArrayList<>();
    dataList.add(TEST_DATA);

    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig config = objectMapper.readValue(TEST_CONFIG, DeidMaskingConfig.class);
    AddressMaskingProviderConfig provider = new AddressMaskingProviderConfig();
    provider.setPostalCodeNearestK(-1);
    ArrayList<MaskingProviderConfig> providers = new ArrayList<>();
    providers.add(provider);
    config.getRules().add(1, new Rule("invalidRuleX", providers));

    DataMaskingModel dataMaskingModel = new DataMaskingModel(
        objectMapper.writeValueAsString(config), dataList, ConfigSchemaType.FHIR);
    String request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: the first masking provider in `maskingProviders` for the rule with `name` value `invalidRuleX` in `rules` is not valid: `postalCodeNearestK` must be greater than 0")));

    providers.add(0, new GenderMaskingProviderConfig());

    dataMaskingModel = new DataMaskingModel(objectMapper.writeValueAsString(config), dataList,
        ConfigSchemaType.FHIR);
    request = objectMapper.writeValueAsString(dataMaskingModel);

    log.info(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isBadRequest())
        .andExpect(content().string(containsString(
            "Invalid masking configuration: the second masking provider in `maskingProviders` for the rule with `name` value `invalidRuleX` in `rules` is not valid: `postalCodeNearestK` must be greater than 0")));
  }
}
