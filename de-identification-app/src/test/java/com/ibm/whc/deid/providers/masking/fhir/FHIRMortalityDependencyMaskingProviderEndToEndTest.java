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
public class FHIRMortalityDependencyMaskingProviderEndToEndTest {

  private final String basePath = "/api/v1";

  private MockMvc mockMvc;

  @Autowired
  private WebApplicationContext wac;

  @Autowired
  public Filter noCacheHeadersFilter;

  private String dataTemplate_;
  private String configDatetimeFirst_;
  private String configBooleanFirst_;

  @Before
  public void setup() throws Exception {
    this.mockMvc = MockMvcBuilders.webAppContextSetup(wac).addFilter(noCacheHeadersFilter).build();
    dataTemplate_ = new String(Files.readAllBytes(Paths.get(getClass()
        .getResource("/data/FHIRMortalityDependencyMaskingProvider.data.template.json").toURI())));
    configDatetimeFirst_ = new String(Files.readAllBytes(Paths.get(getClass()
        .getResource("/config/fhir/FHIRMortalityDependencyMaskingProvider.datetime.first.json")
        .toURI())));
    configBooleanFirst_ = new String(Files.readAllBytes(Paths.get(getClass()
        .getResource("/config/fhir/FHIRMortalityDependencyMaskingProvider.boolean.first.json")
        .toURI())));
  }

  @Test
  public void testDeceasedDateKnownBirthDateOlder() throws Exception {
    for (String config : new String[] {configDatetimeFirst_, configBooleanFirst_}) {
      for (String deceasedDate : new String[] {"\"1929-02-03T11:12:13\"", "\"1930\"",
          "\"1929-03\""}) {
        for (String booleanValue : new String[] {"\"TRue\"", "\"FalSE\"", "true", "false",
            "null"}) {
          String data = dataTemplate_;
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
  }

  @Test
  public void testDeceasedDateKnownBirthDateYounger() throws Exception {
    for (String config : new String[] {configDatetimeFirst_, configBooleanFirst_}) {
      for (String deceasedDate : new String[] {"\"1929-02-02T23:12:13\"", "\"1928-12\"", "1927"}) {
        for (String booleanValue : new String[] {"\"TRue\"", "\"FalSE\"", "true", "false",
            "null"}) {
          String data = dataTemplate_;
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

  @Test
  public void testDeceasedDateKnownBirthDateUnknown() throws Exception {
    // When the birthdate isn't known, the age of the patient can not be determined.
    // Therefore, the patient is always assumed to be young just to be conservative.
    for (String config : new String[] {configDatetimeFirst_, configBooleanFirst_}) {
      for (String birthDate : new String[] {"null", "\"unparseable\"", "\"2020/02/05\"",
          "\"20200205\"", "\"2020-02-05-03-11\""}) {
        for (String deceasedDate : new String[] {"\"1929-02-02T23:12:13\"", "\"1928-12\"", "1927",
            "\"" + LocalDate.now().toString() + "\"", "2200", "\"unparseable\""}) {
          for (String booleanValue : new String[] {"\"TRue\"", "\"FalSE\"", "true", "false",
              "null", "\"invalid\""}) {
            String data = dataTemplate_;
            data = data.replace("{1name}", "birthDate");
            data = data.replace("{1value}", birthDate);
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
  }

  @Test
  public void testDeceasedDateUnknownBooleanDeceasedBirthDateOlder() throws Exception {
    for (String config : new String[] {configDatetimeFirst_, configBooleanFirst_}) {
      LocalDate today = LocalDate.now();
      for (String birthDate : new String[] {today.plusYears(-9L).toString(),
          today.plusYears(-9L).toString().substring(0, 4), today.plusYears(-20L).toString(),
          today.plusYears(-9L).toString().substring(0, 7)}) {

        // string for boolean value
        for (String booleanValue : new String[] {"TRue", "unknown", "falseX", "1", "0"}) {
          String data = dataTemplate_;
          data = data.replace("{1name}", "birthDate");
          data = data.replace("{1value}", "\"" + birthDate + "\"");
          data = data.replace("{2name}", "deceasedDateTimeX");
          data = data.replace("{2value}", "\"x\"");
          data = data.replace("{3name}", "deceasedBoolean");
          data = data.replace("{3value}", "\"" + booleanValue + "\"");
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
              .andExpect(jsonPath("$.data[0].deceasedBoolean").value(equalTo(booleanValue)))
              .andExpect(jsonPath("$.data[0].deceasedDateTime").doesNotHaveJsonPath());
        }

        // boolean for boolean value
        String data = dataTemplate_;
        data = data.replace("{1name}", "birthDate");
        data = data.replace("{1value}", "\"" + birthDate + "\"");
        data = data.replace("{2name}", "deceasedDateTimeX");
        data = data.replace("{2value}", "\"x\"");
        data = data.replace("{3name}", "deceasedBoolean");
        data = data.replace("{3value}", "true");
        System.out.println(data);

        List<String> inputList = new ArrayList<>();
        inputList.add(data);
        DataMaskingModel dataMaskingModel =
            new DataMaskingModel(config, inputList, ConfigSchemaType.FHIR);
        String request = new ObjectMapper().writeValueAsString(dataMaskingModel);

        this.mockMvc
            .perform(post(basePath + "/deidentification")
                .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
            .andDo(print()).andExpect(status().isOk())
            .andExpect(jsonPath("$.data[0].deceasedBoolean").value(equalTo(Boolean.TRUE)))
            .andExpect(jsonPath("$.data[0].deceasedDateTime").doesNotHaveJsonPath());
      }
    }
  }

  @Test
  public void testDeceasedDateUnknownBooleanDeceasedBirthDateYounger() throws Exception {
    for (String config : new String[] {configDatetimeFirst_, configBooleanFirst_}) {
      LocalDate today = LocalDate.now();
      for (String birthDate : new String[] {today.plusYears(-9L).plusDays(1L).toString(),
          today.plusYears(-9L).plusMonths(1L).toString(),
          today.plusYears(-8L).toString().substring(0, 4), today.plusYears(-2L).toString(),
          today.plusYears(-9L).plusMonths(1L).toString().substring(0, 7)}) {

        // string for boolean value
        for (String booleanValue : new String[] {"TRue", "unknown", "falseX", "1", "0"}) {
          String data = dataTemplate_;
          data = data.replace("{1name}", "birthDate");
          data = data.replace("{1value}", "\"" + birthDate + "\"");
          data = data.replace("{2name}", "deceasedDateTimeX");
          data = data.replace("{2value}", "\"x\"");
          data = data.replace("{3name}", "deceasedBoolean");
          data = data.replace("{3value}", "\"" + booleanValue + "\"");
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
              .andExpect(jsonPath("$.data[0].deceasedDateTime").doesNotHaveJsonPath());
        }

        // boolean for boolean value
        String data = dataTemplate_;
        data = data.replace("{1name}", "birthDate");
        data = data.replace("{1value}", "\"" + birthDate + "\"");
        data = data.replace("{2name}", "deceasedDateTimeX");
        data = data.replace("{2value}", "\"x\"");
        data = data.replace("{3name}", "deceasedBoolean");
        data = data.replace("{3value}", "true");
        System.out.println(data);

        List<String> inputList = new ArrayList<>();
        inputList.add(data);
        DataMaskingModel dataMaskingModel =
            new DataMaskingModel(config, inputList, ConfigSchemaType.FHIR);
        String request = new ObjectMapper().writeValueAsString(dataMaskingModel);

        this.mockMvc
            .perform(post(basePath + "/deidentification")
                .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
            .andDo(print()).andExpect(status().isOk())
            .andExpect(jsonPath("$.data[0].deceasedBoolean").value(nullValue()))
            .andExpect(jsonPath("$.data[0].deceasedDateTime").doesNotHaveJsonPath());
      }
    }
  }

  @Test
  public void testDeceasedDateUnknownBooleanDeceasedBirthDateUnknown() throws Exception {
    // When the birthdate isn't known, the age of the patient can not be determined.
    // Therefore, the patient is always assumed to be young just to be conservative.
    for (String config : new String[] {configDatetimeFirst_, configBooleanFirst_}) {
      
      // string for boolean value
      for (String booleanValue : new String[] {"TRue", "unknown", "falseX", "1", "0"}) {
        String data = dataTemplate_;
        data = data.replace("{1name}", "birthDate");
        data = data.replace("{1value}", "null");
        data = data.replace("{2name}", "deceasedDateTime");
        data = data.replace("{2value}", "null");
        data = data.replace("{3name}", "deceasedBoolean");
        data = data.replace("{3value}", "\"" + booleanValue + "\"");
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

      // boolean for boolean value
      String data = dataTemplate_;
      data = data.replace("{1name}", "birthDateX");
      data = data.replace("{1value}", "\"X\"");
      data = data.replace("{2name}", "deceasedDateTimeX");
      data = data.replace("{2value}", "\"x\"");
      data = data.replace("{3name}", "deceasedBoolean");
      data = data.replace("{3value}", "true");
      System.out.println(data);

      List<String> inputList = new ArrayList<>();
      inputList.add(data);
      DataMaskingModel dataMaskingModel =
          new DataMaskingModel(config, inputList, ConfigSchemaType.FHIR);
      String request = new ObjectMapper().writeValueAsString(dataMaskingModel);

      this.mockMvc
          .perform(post(basePath + "/deidentification")
              .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
          .andDo(print()).andExpect(status().isOk())
          .andExpect(jsonPath("$.data[0].deceasedBoolean").value(nullValue()))
          .andExpect(jsonPath("$.data[0].deceasedDateTime").doesNotHaveJsonPath());
    }
  }

  @Test
  public void testDeceasedDateUnknownBooleanFalse() throws Exception {
    for (String config : new String[] {configDatetimeFirst_, configBooleanFirst_}) {
      // birthDate isn't used in this scenario and can be anything
      for (String birthDate : new String[] {"\"2022-01-12\"", "\"invalid\"", "null"}) {
        // string for boolean value
        String data = dataTemplate_;
        data = data.replace("{1name}", "birthDate");
        data = data.replace("{1value}", birthDate);
        data = data.replace("{2name}", "deceasedDateTimeX");
        data = data.replace("{2value}", "\"x\"");
        data = data.replace("{3name}", "deceasedBoolean");
        data = data.replace("{3value}", "\"FalsE\"");
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
            .andExpect(jsonPath("$.data[0].deceasedBoolean").value(equalTo("FalsE")))
            .andExpect(jsonPath("$.data[0].deceasedDateTime").doesNotHaveJsonPath());

        // boolean for boolean value
        data = dataTemplate_;
        data = data.replace("{1name}", "birthDate");
        data = data.replace("{1value}", birthDate);
        data = data.replace("{2name}", "deceasedDateTime");
        data = data.replace("{2value}", "null");
        data = data.replace("{3name}", "deceasedBoolean");
        data = data.replace("{3value}", "false");
        System.out.println(data);

        inputList = new ArrayList<>();
        inputList.add(data);
        dataMaskingModel = new DataMaskingModel(config, inputList, ConfigSchemaType.FHIR);
        request = new ObjectMapper().writeValueAsString(dataMaskingModel);

        this.mockMvc
            .perform(post(basePath + "/deidentification")
                .contentType(MediaType.APPLICATION_JSON_VALUE).content(request))
            .andDo(print()).andExpect(status().isOk())
            .andExpect(jsonPath("$.data[0].deceasedBoolean").value(equalTo(Boolean.FALSE)))
            .andExpect(jsonPath("$.data[0].deceasedDateTime").value(nullValue()));
      }
    }
  }

  @Test
  public void testWrongTargetProperty() throws Exception {
    String config = new String(Files.readAllBytes(Paths.get(getClass()
        .getResource("/config/fhir/FHIRMortalityDependencyMaskingProvider.wrong.target.json")
        .toURI())));
    String data = dataTemplate_;
    data = data.replace("{1name}", "birthDate");
    data = data.replace("{1value}", "\"1980-10-28\"");
    data = data.replace("{2name}", "deceasedDateTime");
    data = data.replace("{2value}", "null");
    data = data.replace("{3name}", "DECEASEDBoolean");
    data = data.replace("{3value}", "false");

    List<String> inputList = new ArrayList<>();
    inputList.add(data);
    DataMaskingModel dataMaskingModel =
        new DataMaskingModel(config, inputList, ConfigSchemaType.FHIR);
    ObjectMapper mapper = new ObjectMapper();
    String request = mapper.writeValueAsString(dataMaskingModel);

    this.mockMvc.perform(post(basePath + "/deidentification")
        .contentType(MediaType.APPLICATION_JSON_VALUE).content(request)).andDo(print())
        .andExpect(status().is(400)).andExpect(content().string(
            "Invalid target property `DECEASEDBoolean` for masking rule `mortality`.  Processing must stop."));
  }

  @Test
  public void testWrongParentNodeType() throws Exception {
    String data = dataTemplate_;
    data = data.replace("{1name}", "birthDate");
    data = data.replace("{1value}", "\"1980-10-28\"");
    data = data.replace("{2name}", "deceasedDateTime");
    data = data.replace("{2value}", "null");
    data = data.replace("{3name}", "deceasedBoolean");
    data = data.replace("{3value}", "[false]");

    List<String> inputList = new ArrayList<>();
    inputList.add(data);
    DataMaskingModel dataMaskingModel =
        new DataMaskingModel(configDatetimeFirst_, inputList, ConfigSchemaType.FHIR);
    ObjectMapper mapper = new ObjectMapper();
    String request = mapper.writeValueAsString(dataMaskingModel);

    this.mockMvc
        .perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(request))
        .andDo(print())
        .andExpect(status().is(400)).andExpect(content().string(
            "Invalid target property `deceasedBoolean[0]` for masking rule `mortality`.  Processing must stop."));
  }
}
