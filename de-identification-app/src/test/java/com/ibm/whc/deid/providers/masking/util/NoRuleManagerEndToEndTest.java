/*
 * © Merative US L.P. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.util;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDate;
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
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.app.endpoint.Application;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.FHIRMortalityDependencyMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.masking.DataMaskingModel;

@RunWith(SpringRunner.class)
// force using a test profile to avoid using any other active profile
// we do not have a real application-test.properties.
@ActiveProfiles(profiles = {"test"})
@AutoConfigureMockMvc
@SpringBootTest(classes = Application.class)
public class NoRuleManagerEndToEndTest {

  private final String basePath = "/api/v1";

  private MockMvc mockMvc;

  @Autowired
  private WebApplicationContext wac;

  @Autowired
  public Filter noCacheHeadersFilter;

  @Before
  public void setup() throws Exception {
    this.mockMvc = MockMvcBuilders.webAppContextSetup(wac).addFilter(noCacheHeadersFilter).build();
  }

  @Test
  public void testMain() throws Exception {
    String config = new String(Files.readAllBytes(
        Paths.get(getClass().getResource("/config/NoRuleManager.end-to-end.config.json").toURI())));
    String data = new String(Files.readAllBytes(
        Paths.get(getClass().getResource("/data/NoRuleManager.end-to-end.data.json").toURI())));
    String resultNotNulled = new String(Files.readAllBytes(Paths
        .get(getClass().getResource("/data/NoRuleManager.end-to-end.data.result.json").toURI())));
    String resultNulled = new String(Files.readAllBytes(Paths.get(
        getClass().getResource("/data/NoRuleManager.end-to-end.data.result.null.json").toURI())));

    List<String> inputList = new ArrayList<>();
    inputList.add(data);
    DataMaskingModel dataMaskingModel =
        new DataMaskingModel(config, inputList, ConfigSchemaType.FHIR);
    ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
    String request = mapper.writeValueAsString(dataMaskingModel);

    MvcResult mvcResult =
        this.mockMvc
            .perform(post(basePath + "/deidentification")
                .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
            .andDo(print()).andExpect(status().isOk()).andReturn();
    String responseBody = mvcResult.getResponse().getContentAsString();
    assertEquals(resultNotNulled.trim(), responseBody.trim());

    DeidMaskingConfig configObj = mapper.readValue(config, DeidMaskingConfig.class);
    configObj.setDefaultNoRuleResolution(false);
    config = mapper.writeValueAsString(configObj);

    dataMaskingModel = new DataMaskingModel(config, inputList, ConfigSchemaType.FHIR);
    request = mapper.writeValueAsString(dataMaskingModel);

    mvcResult =
        this.mockMvc
            .perform(post(basePath + "/deidentification")
                .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
            .andDo(print()).andExpect(status().isOk()).andReturn();
    responseBody = mvcResult.getResponse().getContentAsString();
    assertEquals(resultNulled.trim(), responseBody.trim());
  }
}
