/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import java.nio.file.Files;
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
import com.ibm.whc.deid.app.endpoint.Application;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;
import com.ibm.whc.deid.shared.pojo.masking.DataMaskingModel;

@RunWith(SpringRunner.class)
// force using a test profile to avoid using any other active profile
// we do not have a real application-test.properties.
@ActiveProfiles(profiles = {"test"})
@AutoConfigureMockMvc
@SpringBootTest(classes = Application.class)
public class FHIRMortalityDependencyMaskingProviderTest {

  private final String basePath = "/api/v1";

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
    String data = new String(Files.readAllBytes(Paths.get(getClass()
        .getResource("/data/FHIRMortalityDependencyMaskingProvider.data.template.json").toURI())));
    data = data.replace("{1name}", "birthDate");
    data = data.replace("{1value}", "\"1920-02-03\"");
    data = data.replace("{2name}", "deceasedDateTime");
    data = data.replace("{2value}", "\"2020-02-03T11:12:13\"");
    data = data.replace("{3name}", "deceasedBoolean");
    data = data.replace("{3value}", "false");
    System.out.println(data);

    String config = new String(Files.readAllBytes(
        Paths.get(getClass()
            .getResource("/config/fhir/FHIRMortalityDependencyMaskingProvider.datetime.first.json")
            .toURI())));

    List<String> inputList = new ArrayList<>();
    inputList.add(data);
    DataMaskingModel dataMaskingModel =
        new DataMaskingModel(config, inputList, ConfigSchemaType.FHIR);
    ObjectMapper mapper = new ObjectMapper();
    String request = mapper.writeValueAsString(dataMaskingModel);

    this.mockMvc
        .perform(post(basePath + "/deidentification")
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
        .andDo(print()).andExpect(status().isOk()) // .andDo(MockMvcResultHandlers.print())
        .andExpect(jsonPath("$.data[0].deceasedBoolean").value(equalTo(Boolean.TRUE)))
        .andExpect(jsonPath("$.data[0].deceasedDateTime").value(nullValue()));
  }
}
