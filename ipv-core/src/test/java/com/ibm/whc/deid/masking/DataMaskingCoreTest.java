/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.masking;

import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;
import org.junit.Before;
import org.junit.Test;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;
import com.ibm.whc.deid.shared.pojo.masking.ReferableData;

public class DataMaskingCoreTest {

  private String maskingConf = null;
  private String maskingConf_generic = null;

  private static List<ReferableData> convertList(List<String> inputList) {
    return inputList.stream().map(input -> {
      return new ReferableData(input);
    }).collect(Collectors.toList());
  }

  @Before
  public void setup() throws IOException {
    try (
        InputStream inputStream =
            this.getClass().getResourceAsStream("/config/fhir/masking_config.json");
        Scanner scanner = new Scanner(inputStream, "UTF-8")) {
      maskingConf = scanner.useDelimiter("\\A").next();
    }
    try (
        InputStream inputStream =
            this.getClass().getResourceAsStream("/config/generic/masking_config.json");
        Scanner scanner = new Scanner(inputStream, "UTF-8")) {
      maskingConf_generic = scanner.useDelimiter("\\A").next();
    }
  }

  /**
   * Test a config with invalid parameter name.
   *
   * @throws IOException
   * @throws DeidException
   */
  @Test(expected = DeidException.class)
  public void testInvalidConfig() throws IOException, DeidException {
    DataMaskingCore dataMask = new DataMaskingCore();
    String invalidConfig;
    try (
        InputStream inputStream = this.getClass()
            .getResourceAsStream("/config/fhir/masking_config_incorrect_rule_name.json");
        Scanner scanner = new Scanner(inputStream, "UTF-8")) {
      invalidConfig = scanner.useDelimiter("\\A").next();
    }

    String patientData = null;

    try (InputStream inputStream = this.getClass().getResourceAsStream("/fhir/patientExample.json");
        Scanner scanner = new Scanner(inputStream, "UTF-8")) {
      patientData = scanner.useDelimiter("\\A").next();
    }
    try {
      dataMask.maskData(invalidConfig, Arrays.asList(new ReferableData(patientData)),
          ConfigSchemaType.FHIR);
    } catch (DeidException e) {
      assertTrue(e.getMessage().contains("Unrecognized field \"INVALID_rules\""));
      throw e;
    }
  }

  /**
   * Test a JSON config that points to a rule that does not exist.
   *
   * @throws IOException
   * @throws DeidException
   */
  @Test(expected = DeidException.class)
  public void testInvalidConfig_missing_rule() throws IOException, DeidException {
    DataMaskingCore dataMask = new DataMaskingCore();
    String invalidConfig;
    try (
        InputStream inputStream =
            this.getClass().getResourceAsStream("/config/fhir/masking_config_missing_rule.json");
        Scanner scanner = new Scanner(inputStream, "UTF-8")) {
      invalidConfig = scanner.useDelimiter("\\A").next();
    }

    String patientData = null;

    try (InputStream inputStream = this.getClass().getResourceAsStream("/fhir/patientExample.json");
        Scanner scanner = new Scanner(inputStream, "UTF-8")) {
      patientData = scanner.useDelimiter("\\A").next();
    }
    try {
      dataMask.maskData(invalidConfig, Arrays.asList(new ReferableData(patientData)),
          ConfigSchemaType.FHIR);
    } catch (DeidException e) {
      assertTrue(e.getMessage().contains(
          "the rule assignment with `rule` value `NO_SUCH_RULE` does not refer to a valid rule"));
      throw e;
    }
  }

  @Test
  public void testMaskPatient() throws IOException, DeidException {
    DataMaskingCore dataMask = new DataMaskingCore();

    String patientData = null;
    List<String> inputList = new ArrayList<>();

    try (InputStream inputStream = this.getClass().getResourceAsStream("/fhir/patientExample.json");
        Scanner scanner = new Scanner(inputStream, "UTF-8")) {
      patientData = scanner.useDelimiter("\\A").next();
    }
    inputList.add(patientData);
    List<ReferableData> maskedDataList = dataMask.maskData(maskingConf,
        DataMaskingCoreTest.convertList(inputList), ConfigSchemaType.FHIR);
    String maskedData = maskedDataList.get(0).getData();
    ObjectMapper objectMapper = new ObjectMapper();
    JsonNode maskedNode = objectMapper.readTree(maskedData);

    assertFalse(maskedNode.path("address").get(0).path("city").textValue().isEmpty());
    assertThat(maskedNode.path("address").get(0).path("city").textValue(), not("PleasantVille"));

    System.out.println("Masked output is: " + maskedNode.toString());
  }

  @Test
  public void testMaskDevice() throws IOException, DeidException {
    DataMaskingCore dataMask = new DataMaskingCore();

    String deviceData = null;
    List<String> inputList = new ArrayList<>();

    try (
        InputStream inputStream =
            DataMaskingCore.class.getResourceAsStream("/fhir/deviceExample.json");
        Scanner scanner = new Scanner(inputStream, "UTF-8")) {
      deviceData = scanner.useDelimiter("\\A").next();
    }
    inputList.add(deviceData);

    List<ReferableData> maskedDataList = dataMask.maskData(maskingConf,
        DataMaskingCoreTest.convertList(inputList), ConfigSchemaType.FHIR);

    ObjectMapper objectMapper = new ObjectMapper();
    JsonNode maskedNode = objectMapper.readTree(maskedDataList.get(0).getData());

    // Make sure the id field is not accidentally masked
    assertEquals("example", maskedNode.path("id").textValue());

    assertTrue(maskedNode.path("identifier").get(0).path("system").isNull());
    assertTrue(maskedNode.path("identifier").get(0).path("value").isNull());
    assertTrue(maskedNode.path("identifier").get(1).path("type").path("text").isNull());
    assertTrue(maskedNode.path("identifier").get(1).path("value").isNull());
    assertThat(maskedNode.path("patient").path("reference").textValue(),
        not("Patient/2.16.840.1.113883.19.5"));
  }

  @Test
  public void testMaskPatient_Generic() throws IOException, DeidException {
    DataMaskingCore dataMask = new DataMaskingCore();

    String patientData = null;
    List<String> inputList = new ArrayList<>();

    try (InputStream inputStream = this.getClass().getResourceAsStream("/fhir/patientExample.json");
        Scanner scanner = new Scanner(inputStream, "UTF-8")) {
      patientData = scanner.useDelimiter("\\A").next();
    }
    inputList.add(patientData);

    List<ReferableData> maskedDataList = dataMask.maskData(maskingConf_generic,
        DataMaskingCoreTest.convertList(inputList), ConfigSchemaType.GEN);

    ObjectMapper objectMapper = new ObjectMapper();
    JsonNode maskedNode = objectMapper.readTree(maskedDataList.get(0).getData());

    assertFalse(maskedNode.path("address").get(0).path("city").textValue().isEmpty());
    assertThat(maskedNode.path("address").get(0).path("city").textValue(), not("PleasantVille"));
  }

  @Test
  public void testMaskDevice_Generic() throws IOException, DeidException {
    DataMaskingCore dataMask = new DataMaskingCore();

    String deviceData = null;
    List<String> inputList = new ArrayList<>();

    try (InputStream inputStream = this.getClass().getResourceAsStream("/fhir/deviceExample.json");
        Scanner scanner = new Scanner(inputStream, "UTF-8")) {
      deviceData = scanner.useDelimiter("\\A").next();
    }
    inputList.add(deviceData);

    List<ReferableData> maskedDataList = dataMask.maskData(maskingConf_generic,
        DataMaskingCoreTest.convertList(inputList), ConfigSchemaType.GEN);
    ObjectMapper objectMapper = new ObjectMapper();
    JsonNode maskedNode = objectMapper.readTree(maskedDataList.get(0).getData());

    // Make sure the id field is not accidentally masked
    assertEquals("example", maskedNode.path("id").textValue());

    assertTrue(maskedNode.path("identifier").get(0).path("system").isNull());
    assertTrue(maskedNode.path("identifier").get(0).path("value").isNull());
    assertTrue(maskedNode.path("identifier").get(1).path("type").path("text").isNull());
    assertTrue(maskedNode.path("identifier").get(1).path("value").isNull());
    assertThat(maskedNode.path("patient").path("reference").textValue(),
        not("Patient/2.16.840.1.113883.19.5"));
  }
}
