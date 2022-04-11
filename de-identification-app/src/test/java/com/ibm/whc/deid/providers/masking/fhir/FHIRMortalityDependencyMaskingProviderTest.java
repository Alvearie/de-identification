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

    String dataTemplate = new String(Files.readAllBytes(Paths.get(getClass()
        .getResource("/data/FHIRMortalityDependencyMaskingProvider.data.template.json").toURI())));


    String config = new String(Files.readAllBytes(Paths.get(getClass()
        .getResource("/config/fhir/FHIRMortalityDependencyMaskingProvider.datetime.first.json")
        .toURI())));

    runTests(config, dataTemplate);

    config = new String(Files.readAllBytes(Paths.get(getClass()
        .getResource("/config/fhir/FHIRMortalityDependencyMaskingProvider.boolean.first.json")
        .toURI())));

    runTests(config, dataTemplate);
  }

  protected void runTests(String config, String data) throws Exception {
    runBirthDateKnownDeceasedDateKnownOlder(config, data);
  }

  protected void runBirthDateKnownDeceasedDateKnownOlder(String config, String template)
      throws Exception {
    String[] deceasedDates = new String[] {"\"1929-02-03T11:12:13\"", "\"1930\"", "\"1929-03\""};
    for (String deceasedDate : deceasedDates) {
      for (String booleanValue : new String[] {"\"TRue\"", "\"FalSE\"", "true", "false", "null"}) {
        String data = template;
        data = data.replace("{1name}", "birthDate");
        data = data.replace("{1value}", "\"1920-02-03\"");
        data = data.replace("{2name}", "deceasedDateTime");
        data = data.replace("{2value}", deceasedDate);
        data = data.replace("{3name}", "deceasedBoolean");
        data = data.replace("{3value}", booleanValue);
        System.out.println(data);

        List<String> inputList = new ArrayList<>();
        inputList.add(data);
        DataMaskingModel dataMaskingModel =
            new DataMaskingModel(config, inputList, ConfigSchemaType.FHIR);
        ObjectMapper mapper = new ObjectMapper();
        String request = mapper.writeValueAsString(dataMaskingModel);

        this.mockMvc
            .perform(post(basePath + "/deidentification")
                .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
            .andDo(print()).andExpect(status().isOk())
            .andExpect(jsonPath("$.data[0].deceasedBoolean").value(equalTo(Boolean.TRUE)))
            .andExpect(jsonPath("$.data[0].deceasedDateTime").value(nullValue()));
      }
    }
  }

  protected void runBirthDateKnownDeceasedDateKnownYounger(String config, String template)
      throws Exception {
    String[] deceasedDates = new String[] {"\"1929-02-02T23:12:13\"", "\"1928-12\"", "1927"};
    for (String deceasedDate : deceasedDates) {
      for (String booleanValue : new String[] {"\"TRue\"", "\"FalSE\"", "true", "false", "null"}) {
        String data = template;
        data = data.replace("{1name}", "birthDate");
        data = data.replace("{1value}", "\"1920-02-03\"");
        data = data.replace("{2name}", "deceasedDateTime");
        data = data.replace("{2value}", deceasedDate);
        data = data.replace("{3name}", "deceasedBoolean");
        data = data.replace("{3value}", booleanValue);
        System.out.println(data);

        List<String> inputList = new ArrayList<>();
        inputList.add(data);
        DataMaskingModel dataMaskingModel =
            new DataMaskingModel(config, inputList, ConfigSchemaType.FHIR);
        ObjectMapper mapper = new ObjectMapper();
        String request = mapper.writeValueAsString(dataMaskingModel);

        this.mockMvc
            .perform(post(basePath + "/deidentification")
                .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
            .andDo(print()).andExpect(status().isOk())
            .andExpect(jsonPath("$.data[0].deceasedBoolean").value(nullValue()))
            .andExpect(jsonPath("$.data[0].deceasedDateTime").value(nullValue()));
      }
    }
  }
}
