/*
 * © Merative US L.P. 2016, 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint.datamasking;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import org.springframework.stereotype.Service;
import com.ibm.whc.deid.masking.DataMaskingCore;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.ReferableData;

@Service
public class DataMaskingService {

  private final DataMaskingCore dataMaskingCore = new DataMaskingCore();

  /**
   * @param configuration masking configuration
   * @param list JSON documents to process
   * 
   * @return the processed JSON documents in string format
   */
  public final List<String> maskData(final DeidMaskingConfig configuration, final List<String> list,
      ConfigSchemaTypes schemaType) {
    List<String> outputRecords = new ArrayList<>();
    AtomicInteger messageOrder = new AtomicInteger();
    outputRecords
        .addAll(dataMaskingCore.maskData(configuration, list.stream().map(input -> {
          return new ReferableData(String.valueOf(messageOrder.getAndIncrement()), input);
        }).collect(Collectors.toList()), schemaType).stream().map(input -> {
          return input.getData();
        }).collect(Collectors.toList()));
    return outputRecords;
  }
}
