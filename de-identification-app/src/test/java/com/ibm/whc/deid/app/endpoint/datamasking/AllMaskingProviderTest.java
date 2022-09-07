/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint.datamasking;

import static org.junit.Assert.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import javax.servlet.Filter;
import org.junit.Before;
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
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.result.MockMvcResultHandlers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.app.endpoint.Application;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;
import com.ibm.whc.deid.shared.pojo.masking.DataMaskingModel;

@RunWith(SpringRunner.class)
// force using a test profile to avoid using any other active profile
// we do not have a real application-test.properties.
@ActiveProfiles(profiles = {"test"})
@AutoConfigureMockMvc
@SpringBootTest(classes = Application.class)
public class AllMaskingProviderTest {

  private final String basePath = "/api/v1";

  private MockMvc mockMvc;

  private static final Logger log = LoggerFactory.getLogger(DataMaskingControllerTest.class);

  @Autowired
  private WebApplicationContext wac;

  @Autowired
  public Filter noCacheHeadersFilter;

  @Before
  public void setup() {
    this.mockMvc = MockMvcBuilders.webAppContextSetup(wac).addFilter(noCacheHeadersFilter).build();
  }

  @Test
  public void testMaskData() throws Exception {
    String data = new String(Files.readAllBytes(
        Paths.get(getClass().getResource("/masking/data/TestMaskingProvider_Input.json").toURI())));

    Path resPath = Paths
        .get(getClass().getResource("/config/generic/TestMaskingProvider_masking.json").toURI());
    String config = new String(Files.readAllBytes(resPath), StandardCharsets.UTF_8);

    List<String> inputList = new ArrayList<>();
    inputList.add(data);
    DataMaskingModel dataMaskingModel =
        new DataMaskingModel(config, inputList, ConfigSchemaType.GEN);
    ObjectMapper mapper = new ObjectMapper();
    String request = mapper.writeValueAsString(dataMaskingModel);
    JsonNode inputNode = mapper.readTree(data);

    log.info(request);
    MvcResult result = this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isOk()).andDo(MockMvcResultHandlers.print()).andReturn();

    // Store the results in a jsonNode
    String resultJson = result.getResponse().getContentAsString();
    JsonNode resultNode = mapper.readTree(resultJson);
    JsonNode outputNode = resultNode.get("data");
    assertTrue("Masking provider test is passed",
        compareMaskingProvider(inputNode, outputNode.get(0)));
  }

  private boolean compareMaskingProvider(JsonNode inputNode, JsonNode outputNode)
      throws DeidException, IOException {

    // Store the input json into a map
    Map<String, String> mapInput = new HashMap<>();
    Iterator<Entry<String, JsonNode>> expectedKeysInput = inputNode.fields();
    String inputValue;
    while (expectedKeysInput.hasNext()) {
      Entry<String, JsonNode> entry = expectedKeysInput.next();
      inputValue = entry.getValue().asText(null);
      mapInput.put(entry.getKey(), inputValue);
    }
    log.info("value Input: " + mapInput);

    // Store the output json into a map
    Map<String, String> mapOutput = new HashMap<>();
    Iterator<Entry<String, JsonNode>> expectedKeysOutput = outputNode.fields();
    String outputValue;
    while (expectedKeysOutput.hasNext()) {
      Entry<String, JsonNode> entry = expectedKeysOutput.next();
      outputValue = entry.getValue().asText(null);
      mapOutput.put(entry.getKey(), outputValue);
    }
    log.info("value output: " + mapOutput);

    // Comparing input and output values
    MaskingProviderOutputComparator mp = new MaskingProviderOutputComparator();
    boolean test = mp.CompareMap(mapInput, mapOutput);
    return test;
  }
}
