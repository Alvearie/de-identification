/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;
import com.ibm.whc.deid.shared.pojo.masking.ReferableData;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import org.junit.Test;

public class DataMaskingCoreTest2 {

  private static List<ReferableData> convertList(List<String> inputList) {
    return inputList.stream().map(input -> {
      return new ReferableData(input);
    }).collect(Collectors.toList());
  }

  @Test
  public void testIdenticalStringArrayMasking() {
    try {
      DataMaskingCore dataMask = new DataMaskingCore();

      String config = new String(
          Files.readAllBytes(
              Paths.get(getClass().getResource("/config/identical-array.json").toURI())),
          StandardCharsets.UTF_8);

      List<String> inputList = new ArrayList<>();
      inputList.add(new String(
          Files.readAllBytes(
              Paths.get(getClass().getResource("/data/identical-array.json").toURI())),
          StandardCharsets.UTF_8));

      List<ReferableData> maskedDataList = dataMask.maskData(config,
          DataMaskingCoreTest2.convertList(inputList), ConfigSchemaType.GEN);

      assertEquals(1, maskedDataList.size());
      assertEquals("{\"address\":[{\"line\":[null,\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"]}]}", maskedDataList.get(0).getData()); 

    } catch (Exception e) {
      e.printStackTrace();
      fail(e.getClass().getName() + ":" + e.getMessage());
    }
  }
}
