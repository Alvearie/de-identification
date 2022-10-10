/*
 * © Merative US L.P. 2016, 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint.datamasking;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.not;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import javax.servlet.Filter;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
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
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.app.endpoint.Application;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;
import com.ibm.whc.deid.shared.pojo.masking.DataMaskingModel;
import com.ibm.whc.deid.shared.pojo.masking.DataMaskingObjectModel;
import com.ibm.whc.deid.shared.util.MaskingConfigUtils;

@RunWith(SpringRunner.class)
// force using a test profile to avoid using any other active profile
// we do not have a real application-test.properties.
@ActiveProfiles(profiles = {"test"})
@AutoConfigureMockMvc
@SpringBootTest(classes = Application.class)
public class DataMaskingControllerTest {

  private static final ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();

  private static final String basePath = "/api/v1";

  private MockMvc mockMvc;

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
    String data = new String(Files
        .readAllBytes(Paths.get(getClass().getResource("/masking/data/simple_fhir.json").toURI())));
    String config = new String(Files.readAllBytes(
        Paths.get(getClass().getResource("/config/fhir/masking_config.json").toURI())));

    List<String> inputList = new ArrayList<>();
    inputList.add(data);
    DataMaskingModel dataMaskingModel =
        new DataMaskingModel(config, inputList, ConfigSchemaType.FHIR);
    String request = mapper.writeValueAsString(dataMaskingModel);

    System.out.println(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isOk())
        .andExpect(header().string("cache-control", "no-cache, no-store, no-transform"))
        .andExpect(header().string("pragma", "no-cache")).andExpect(header().string("expires", "0"))
        .andExpect(jsonPath("$.data[0].id").value(containsString("1234")))
        .andExpect(jsonPath("$.data[0].patient.display").value("XXXXXXXXXXXX"));

    DataMaskingObjectModel model = new DataMaskingObjectModel();
    model.setConfig(MaskingConfigUtils.validateConfig(config));
    model.setSchemaType(ConfigSchemaType.FHIR);
    model.setData((ObjectNode) mapper.readTree(data));
    request = mapper.writeValueAsString(model);

    System.out.println(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification/single")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isOk())
        .andExpect(header().string("cache-control", "no-cache, no-store, no-transform"))
        .andExpect(header().string("pragma", "no-cache")).andExpect(header().string("expires", "0"))
        .andExpect(jsonPath("$.data.id").value(containsString("1234")))
        .andExpect(jsonPath("$.data.patient.display").value("XXXXXXXXXXXX"));
  }

  @Test
  public void testGenericMaskData() throws Exception {
    String data = new String(Files
        .readAllBytes(Paths.get(getClass().getResource("/masking/data/simple_fhir.json").toURI())));
    Path resPath = Paths.get(getClass().getResource("/config/generic/masking_config.json").toURI());
    String config = new String(Files.readAllBytes(resPath), StandardCharsets.UTF_8);

    List<String> inputList = new ArrayList<>();
    inputList.add(data);
    DataMaskingModel dataMaskingModel =
        new DataMaskingModel(config, inputList, ConfigSchemaType.GEN);
    String request = mapper.writeValueAsString(dataMaskingModel);

    System.out.println(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isOk())
        .andExpect(header().string("cache-control", "no-cache, no-store, no-transform"))
        .andExpect(header().string("pragma", "no-cache")).andExpect(header().string("expires", "0"))
        .andExpect(jsonPath("$.data[0].id").value(containsString("1234")))
        .andExpect(jsonPath("$.data[0].patient.display").value("------------"));

    DataMaskingObjectModel model = new DataMaskingObjectModel();
    model.setConfig(MaskingConfigUtils.validateConfig(config));
    model.setSchemaType(ConfigSchemaType.GEN);
    model.setData((ObjectNode) mapper.readTree(data));
    request = mapper.writeValueAsString(model);

    System.out.println(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification/single")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isOk())
        .andExpect(header().string("cache-control", "no-cache, no-store, no-transform"))
        .andExpect(header().string("pragma", "no-cache")).andExpect(header().string("expires", "0"))
        .andExpect(jsonPath("$.data.id").value(containsString("1234")))
        .andExpect(jsonPath("$.data.patient.display").value("------------"));
  }

  @Test
  public void testGenericMaskDataDefaultType() throws Exception {
    String data = new String(Files
        .readAllBytes(Paths.get(getClass().getResource("/masking/data/simple_gen.json").toURI())));
    Path resPath = Paths.get(getClass().getResource("/config/generic/simple_config.json").toURI());
    String config = new String(Files.readAllBytes(resPath), StandardCharsets.UTF_8);
    List<String> inputList = new ArrayList<>();
    inputList.add(data);
    DataMaskingModel dataMaskingModel =
        new DataMaskingModel(config, inputList, ConfigSchemaType.GEN);
    String request = mapper.writeValueAsString(dataMaskingModel);

    System.out.println(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print()).andExpect(status().isOk())
        .andExpect(header().string("cache-control", "no-cache, no-store, no-transform"))
        .andExpect(header().string("pragma", "no-cache")).andExpect(header().string("expires", "0"))
        .andExpect(jsonPath("$.data[0].phone").value(equalTo("00000000")))
        .andExpect(jsonPath("$.data[0].numbers.ssn").value(equalTo("X")));

    DataMaskingObjectModel model = new DataMaskingObjectModel();
    model.setConfig(MaskingConfigUtils.validateConfig(config));
    model.setSchemaType(ConfigSchemaType.GEN);
    model.setData((ObjectNode) mapper.readTree(data));
    request = mapper.writeValueAsString(model);

    System.out.println(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification/single")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isOk())
        .andExpect(header().string("cache-control", "no-cache, no-store, no-transform"))
        .andExpect(header().string("pragma", "no-cache")).andExpect(header().string("expires", "0"))
        .andExpect(jsonPath("$.data.phone").value(equalTo("00000000")))
        .andExpect(jsonPath("$.data.numbers.ssn").value(equalTo("X")));
  }

  @Test
  public void testMaskLocation() throws Exception {
    String maskingFilePath = "/config/fhir/masking_config.json";
    Path resPath = Paths.get(this.getClass().getResource(maskingFilePath).toURI());
    String config = new String(Files.readAllBytes(resPath), "UTF8");
    String data = new String(Files.readAllBytes(
        Paths.get(getClass().getResource("/masking/data/fhir_location1.json").toURI())));
    List<String> inputList = new ArrayList<>();
    inputList.add(data);
    DataMaskingModel dataMaskingModel =
        new DataMaskingModel(config, inputList, ConfigSchemaType.FHIR);
    String request = mapper.writeValueAsString(dataMaskingModel);

    System.out.println(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isOk())
        .andExpect(header().string("cache-control", "no-cache, no-store, no-transform"))
        .andExpect(header().string("pragma", "no-cache")).andExpect(header().string("expires", "0"))
        .andExpect(jsonPath("$.data[0].hashTestOne")
            .value(equalTo("31D03C3D7A9F70E679CA00C047E033D5A29210F6BF5B2FF12B9FC4962C102357")));

    DataMaskingObjectModel model = new DataMaskingObjectModel();
    model.setConfig(MaskingConfigUtils.validateConfig(config));
    model.setSchemaType(ConfigSchemaType.FHIR);
    model.setData((ObjectNode) mapper.readTree(data));
    request = mapper.writeValueAsString(model);

    System.out.println(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification/single")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isOk())
        .andExpect(header().string("cache-control", "no-cache, no-store, no-transform"))
        .andExpect(header().string("pragma", "no-cache")).andExpect(header().string("expires", "0"))
        .andExpect(jsonPath("$.data.hashTestOne")
            .value(equalTo("31D03C3D7A9F70E679CA00C047E033D5A29210F6BF5B2FF12B9FC4962C102357")));
  }

  /**
   * Test masking data with different configuration. Once with HASH masking provider and another
   * time with CONTINENT.
   *
   * <p>
   * Make sure the returned data is correctly masked
   */
  @Test
  public void testCachedMaskingProvider_DifferentConfig() throws Exception {
    String data = new String(Files.readAllBytes(
        Paths.get(getClass().getResource("/masking/data/fhir_location2.json").toURI())));
    List<String> inputList = new ArrayList<>();
    inputList.add(data);

    // Use HASH masking provider
    String hashMaskingConfig = "/config/fhir/masking_config_test_cache.json";
    String config = new String(
        Files.readAllBytes(Paths.get(getClass().getResource(hashMaskingConfig).toURI())));

    DataMaskingModel dataMaskingModel =
        new DataMaskingModel(config, inputList, ConfigSchemaType.FHIR);
    String request = mapper.writeValueAsString(dataMaskingModel);

    System.out.println(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isOk())
        .andExpect(header().string("cache-control", "no-cache, no-store, no-transform"))
        .andExpect(header().string("pragma", "no-cache")).andExpect(header().string("expires", "0"))
        .andExpect(jsonPath("$.data[0].continent")
            .value(equalTo("FF588B4101673E2DFDD21041B6D5BF583703FF5D296FE5C606C5489132220EF7")));

    DataMaskingObjectModel model = new DataMaskingObjectModel();
    model.setConfig(MaskingConfigUtils.validateConfig(config));
    model.setSchemaType(ConfigSchemaType.FHIR);
    model.setData((ObjectNode) mapper.readTree(data));
    request = mapper.writeValueAsString(model);

    System.out.println(request);
    this.mockMvc
        .perform(post(basePath + "/deidentification/single")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isOk())
        .andExpect(header().string("cache-control", "no-cache, no-store, no-transform"))
        .andExpect(header().string("pragma", "no-cache")).andExpect(header().string("expires", "0"))
        .andExpect(jsonPath("$.data.continent")
            .value(equalTo("FF588B4101673E2DFDD21041B6D5BF583703FF5D296FE5C606C5489132220EF7")));

    // Use the CONTINENT masking provider
    String continentMaskingConfig = "/config/fhir/masking_config.json";
    config = new String(
        Files.readAllBytes(Paths.get(getClass().getResource(continentMaskingConfig).toURI())));

    dataMaskingModel = new DataMaskingModel(config, inputList, ConfigSchemaType.FHIR);
    request = mapper.writeValueAsString(dataMaskingModel);

    System.out.println(request);
    // Make sure the continent data is not hashed
    this.mockMvc
        .perform(post(basePath + "/deidentification")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andExpect(status().isOk()).andDo(MockMvcResultHandlers.print())
        .andExpect(header().string("cache-control", "no-cache, no-store, no-transform"))
        .andExpect(header().string("pragma", "no-cache")).andExpect(header().string("expires", "0"))
        .andExpect(jsonPath("$.data[0].continent").value(
            not(equalTo("FF588B4101673E2DFDD21041B6D5BF583703FF5D296FE5C606C5489132220EF7"))));

    model = new DataMaskingObjectModel();
    model.setConfig(MaskingConfigUtils.validateConfig(config));
    model.setSchemaType(ConfigSchemaType.FHIR);
    model.setData((ObjectNode) mapper.readTree(data));
    request = mapper.writeValueAsString(model);

    System.out.println(request);
    // Make sure the continent data is not hashed
    this.mockMvc
        .perform(post(basePath + "/deidentification/single")
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andExpect(status().isOk()).andDo(MockMvcResultHandlers.print())
        .andExpect(header().string("cache-control", "no-cache, no-store, no-transform"))
        .andExpect(header().string("pragma", "no-cache")).andExpect(header().string("expires", "0"))
        .andExpect(jsonPath("$.data.continent").value(
            not(equalTo("FF588B4101673E2DFDD21041B6D5BF583703FF5D296FE5C606C5489132220EF7"))));
  }
}
