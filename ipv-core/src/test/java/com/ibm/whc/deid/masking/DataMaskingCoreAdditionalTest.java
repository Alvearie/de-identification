/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.ReferableData;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import org.junit.Test;

public class DataMaskingCoreAdditionalTest {

  @Test
  public void testRulesAppliedInAssignmentOrder() throws Exception {
    DataMaskingCore dataMask = new DataMaskingCore();

    String configStr = readStringResource("/config/ruleOrderMaintained.config.json");
    DeidMaskingConfig config =
        ObjectMapperFactory.getObjectMapper().readValue(configStr, DeidMaskingConfig.class);

    List<String> inputList = new ArrayList<>();
    inputList.add(readStringResource("/data/ruleOrderMaintained.data.json"));

    List<ReferableData> maskedDataList =
        dataMask.maskData(config, convertList(inputList), ConfigSchemaType.FHIR);

    String result = readStringResource("/result/ruleOrderMaintained.result.json");
    assertEquals(1, maskedDataList.size());
    assertEquals(result, maskedDataList.get(0).getData());
  }

  @Test
  public void testRulesAppliedInAssignmentOrderNoRule() throws Exception {
    DataMaskingCore dataMask = new DataMaskingCore();

    String config = readStringResource("/config/ruleOrderMaintained.config.json");
    config =
        config.replace("\"defaultNoRuleResolution\": true,", "\"defaultNoRuleResolution\": false,");
    DeidMaskingConfig maskingConfig =
        ObjectMapperFactory.getObjectMapper().readValue(config, DeidMaskingConfig.class);

    List<String> inputList = new ArrayList<>();
    inputList.add(readStringResource("/data/ruleOrderMaintained.data.json"));

    List<ReferableData> maskedDataList =
        dataMask.maskData(maskingConfig, convertList(inputList), ConfigSchemaType.FHIR);

    String result = readStringResource("/result/ruleOrderMaintained.norule.result.json");
    assertEquals(1, maskedDataList.size());
    assertEquals(result, maskedDataList.get(0).getData());
  }

  @Test
  public void testIdenticalStringArrayMasking() throws Exception {
    DataMaskingCore dataMask = new DataMaskingCore();

    String config = readStringResource("/config/identical-array.json");
    DeidMaskingConfig maskingConfig =
        ObjectMapperFactory.getObjectMapper().readValue(config, DeidMaskingConfig.class);

    List<String> inputList = new ArrayList<>();
    inputList.add(readStringResource("/data/identical-array.json"));

    List<ReferableData> maskedDataList =
        dataMask.maskData(maskingConfig, convertList(inputList), ConfigSchemaType.GEN);

    assertEquals(1, maskedDataList.size());
    assertEquals(
        "{\"address\":[{\"line\":[null,\"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\",null,\"3300 University Dr, Suite 19\"]}]}",
        maskedDataList.get(0).getData());
  }

  @Test
  public void testInvalidPathNotLeaf() throws Exception {
    String config = readStringResource("/config/pathNotLeaf.config.json");
    DeidMaskingConfig maskingConfig =
        ObjectMapperFactory.getObjectMapper().readValue(config, DeidMaskingConfig.class);

    List<String> inputList = new ArrayList<>();
    inputList.add(readStringResource("/data/pathNotLeaf.leaf.json"));
    inputList.add(readStringResource("/data/pathNotLeaf.array.json"));
    inputList.add(readStringResource("/data/pathNotLeaf.nested-array.json"));
    inputList.add(readStringResource("/data/pathNotLeaf.object.json"));
    inputList.add(readStringResource("/data/pathNotLeaf.mixed-array.json"));

    String resultLeaf = readStringResource("/result/pathNotLeaf.result.leaf.json");
    String resultArray = readStringResource("/result/pathNotLeaf.result.array.json");
    String resultNestedArray = readStringResource("/result/pathNotLeaf.result.nested-array.json");
    String resultObject = readStringResource("/result/pathNotLeaf.result.object.json");
    String resultMixedArray = readStringResource("/result/pathNotLeaf.result.mixed-array.json");

    DataMaskingCore dataMask = new DataMaskingCore();
    List<ReferableData> maskedDataList =
        dataMask.maskData(maskingConfig, convertList(inputList), ConfigSchemaType.FHIR);

    assertEquals(5, maskedDataList.size());
    assertEquals(resultLeaf, maskedDataList.get(0).getData());
    assertEquals(resultArray, maskedDataList.get(1).getData());
    assertEquals(resultNestedArray, maskedDataList.get(2).getData());
    assertEquals(resultObject, maskedDataList.get(3).getData());
    assertEquals(resultMixedArray, maskedDataList.get(4).getData());
  }

  @Test
  public void testInvalidPathNotLeaf_noRule() throws Exception {
    String config = readStringResource("/config/pathNotLeaf.norule.config.json");
    DeidMaskingConfig maskingConfig =
        ObjectMapperFactory.getObjectMapper().readValue(config, DeidMaskingConfig.class);

    List<String> inputList = new ArrayList<>();
    inputList.add(readStringResource("/data/pathNotLeaf.leaf.json"));
    inputList.add(readStringResource("/data/pathNotLeaf.array.json"));
    inputList.add(readStringResource("/data/pathNotLeaf.nested-array.json"));
    inputList.add(readStringResource("/data/pathNotLeaf.object.json"));
    inputList.add(readStringResource("/data/pathNotLeaf.mixed-array.json"));

    String resultLeaf = readStringResource("/result/pathNotLeaf.result.norule.leaf.json");
    String resultArray = readStringResource("/result/pathNotLeaf.result.norule.array.json");
    String resultNestedArray =
        readStringResource("/result/pathNotLeaf.result.norule.nested-array.json");
    String resultObject = readStringResource("/result/pathNotLeaf.result.norule.object.json");
    String resultMixedArray =
        readStringResource("/result/pathNotLeaf.result.norule.mixed-array.json");

    DataMaskingCore dataMask = new DataMaskingCore();
    List<ReferableData> maskedDataList =
        dataMask.maskData(maskingConfig, convertList(inputList), ConfigSchemaType.FHIR);

    assertEquals(5, maskedDataList.size());
    assertEquals(resultLeaf, maskedDataList.get(0).getData());
    assertEquals(resultArray, maskedDataList.get(1).getData());
    assertEquals(resultNestedArray, maskedDataList.get(2).getData());
    assertEquals(resultObject, maskedDataList.get(3).getData());
    assertEquals(resultMixedArray, maskedDataList.get(4).getData());
  }

  @Test
  public void testNoRule_ResourceTypes() throws Exception {
    String config = readStringResource("/config/defNoRuleRes.config.json");
    DeidMaskingConfig maskingConfig =
        ObjectMapperFactory.getObjectMapper().readValue(config, DeidMaskingConfig.class);

    List<String> inputList = new ArrayList<>();
    inputList.add(readStringResource("/data/defNoRuleRes.type-listed-no-rules.json"));
    inputList.add(readStringResource("/data/defNoRuleRes.type-not-listed.json"));
    inputList.add(readStringResource("/data/defNoRuleRes.type-not-present.json"));
    List<ReferableData> convertedList = convertList(inputList);

    String resultListed =
        readStringResource("/result/defNoRuleRes.result.type-listed-no-rules.json");
    String resultNotListed = readStringResource("/result/defNoRuleRes.result.type-not-listed.json");
    String resultNotPresent =
        readStringResource("/result/defNoRuleRes.result.type-not-present.json");

    DataMaskingCore dataMask = new DataMaskingCore();
    List<ReferableData> maskedDataList =
        dataMask.maskData(maskingConfig, convertedList, ConfigSchemaType.FHIR);

    assertEquals(3, maskedDataList.size());
    boolean foundListed = false;
    boolean foundNotListed = false;
    boolean foundNotPresent = false;
    for (ReferableData rd : maskedDataList) {
      String result = rd.getData();
      if (result.contains("Unknown")) {
        assertEquals(resultNotPresent, result);
        foundNotPresent = true;
      } else if (rd.getData().contains("Device")) {
        assertEquals(resultNotListed, result);
        foundNotListed = true;
      } else {
        assertEquals(resultListed, result);
        foundListed = true;
      }
    }
    assertTrue(foundListed);
    assertTrue(foundNotListed);
    assertTrue(foundNotPresent);
  }

  @Test
  public void testMessageTypesIgnoredWhenDefault() throws Exception {
    String config = readStringResource("/config/generic/messageTypesIgnored.json");
    DeidMaskingConfig maskingConfig =
        ObjectMapperFactory.getObjectMapper().readValue(config, DeidMaskingConfig.class);

    List<String> inputList = new ArrayList<>();
    inputList.add(readStringResource("/data/messageTypesIgnored.json"));
    List<ReferableData> convertedList = convertList(inputList);

    String resultList = readStringResource("/result/messageTypesIgnored.json");

    DataMaskingCore dataMask = new DataMaskingCore();
    List<ReferableData> maskedDataList =
        dataMask.maskData(maskingConfig, convertedList, ConfigSchemaType.GEN);

    assertEquals(1, maskedDataList.size());
    assertEquals(resultList, maskedDataList.get(0).getData());
  }

  private List<ReferableData> convertList(List<String> inputList) {
    return inputList.stream().map(input -> {
      return new ReferableData(input);
    }).collect(Collectors.toList());
  }

  private String readStringResource(String path) throws IOException, URISyntaxException {
    return new String(Files.readAllBytes(Paths.get(getClass().getResource(path).toURI())),
        StandardCharsets.UTF_8);
  }
}
