/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import org.junit.Before;
import org.junit.Test;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.ReferableData;

public class DataMaskingCoreTest {

  private DeidMaskingConfig maskingConf = null;
  private DeidMaskingConfig maskingConf_generic = null;

  private static List<ReferableData> convertList(List<String> inputList) {
    ArrayList<ReferableData> outlist = new ArrayList<>(inputList.size());
    for (String s : inputList) {
      outlist.add(new ReferableData(s));
    }
    return outlist;
  }

  @Before
  public void setup() throws IOException {
    try (
        InputStream inputStream =
            this.getClass().getResourceAsStream("/config/fhir/masking_config.json");
        Scanner scanner = new Scanner(inputStream, "UTF-8")) {
      String s = scanner.useDelimiter("\\A").next();
      maskingConf = ObjectMapperFactory.getObjectMapper().readValue(s, DeidMaskingConfig.class);
    }
    try (
        InputStream inputStream =
            this.getClass().getResourceAsStream("/config/generic/masking_config.json");
        Scanner scanner = new Scanner(inputStream, "UTF-8")) {
      String s = scanner.useDelimiter("\\A").next();
      maskingConf_generic =
          ObjectMapperFactory.getObjectMapper().readValue(s, DeidMaskingConfig.class);
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
    List<ReferableData> maskedDataList = dataMask.maskData(maskingConf, null,
        DataMaskingCoreTest.convertList(inputList), ConfigSchemaType.FHIR);
    String maskedData = maskedDataList.get(0).getData();
    ObjectMapper objectMapper = new ObjectMapper();
    JsonNode maskedNode = objectMapper.readTree(maskedData);

    assertFalse(maskedNode.path("address").get(0).path("city").textValue().isEmpty());
    assertNotEquals("PleasantVille", maskedNode.path("address").get(0).path("city").textValue());

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

    List<ReferableData> maskedDataList = dataMask.maskData(maskingConf, null,
        DataMaskingCoreTest.convertList(inputList), ConfigSchemaType.FHIR);

    ObjectMapper objectMapper = new ObjectMapper();
    JsonNode maskedNode = objectMapper.readTree(maskedDataList.get(0).getData());

    // Make sure the id field is not accidentally masked
    assertEquals("example", maskedNode.path("id").textValue());

    assertTrue(maskedNode.path("identifier").get(0).path("system").isNull());
    assertTrue(maskedNode.path("identifier").get(0).path("value").isNull());
    assertTrue(maskedNode.path("identifier").get(1).path("type").path("text").isNull());
    assertTrue(maskedNode.path("identifier").get(1).path("value").isNull());
    assertNotEquals("Patient/2.16.840.1.113883.19.5",
        maskedNode.path("patient").path("reference").textValue());
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

    List<ReferableData> maskedDataList = dataMask.maskData(maskingConf_generic, null,
        DataMaskingCoreTest.convertList(inputList), ConfigSchemaType.GEN);

    ObjectMapper objectMapper = new ObjectMapper();
    JsonNode maskedNode = objectMapper.readTree(maskedDataList.get(0).getData());

    assertFalse(maskedNode.path("address").get(0).path("city").textValue().isEmpty());
    assertNotEquals("PleasantVille", maskedNode.path("address").get(0).path("city").textValue());
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

    List<ReferableData> maskedDataList = dataMask.maskData(maskingConf_generic, null,
        DataMaskingCoreTest.convertList(inputList), ConfigSchemaType.GEN);
    ObjectMapper objectMapper = new ObjectMapper();
    JsonNode maskedNode = objectMapper.readTree(maskedDataList.get(0).getData());

    // Make sure the id field is not accidentally masked
    assertEquals("example", maskedNode.path("id").textValue());

    assertTrue(maskedNode.path("identifier").get(0).path("system").isNull());
    assertTrue(maskedNode.path("identifier").get(0).path("value").isNull());
    assertTrue(maskedNode.path("identifier").get(1).path("type").path("text").isNull());
    assertTrue(maskedNode.path("identifier").get(1).path("value").isNull());
    assertNotEquals("Patient/2.16.840.1.113883.19.5",
        maskedNode.path("patient").path("reference").textValue());
  }
}
