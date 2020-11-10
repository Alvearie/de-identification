/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint.datamasking;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.springframework.stereotype.Service;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.ibm.whc.deid.masking.DataMaskingCore;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

@Service
public class DataMaskingService {

  private static final LogManager log = LogManager.getInstance();

  private final DataMaskingCore dataMaskingCore = new DataMaskingCore();

  /**
   * @param configuration
   * @param list
   * @return
   * @throws DeidException
   * @throws IOException
   * @throws JsonMappingException
   * @throws JsonParseException
   */
  public final List<String> maskData(final String configuration, final List<String> list,
      ConfigSchemaTypes schemaType)
      throws DeidException, JsonParseException, JsonMappingException, IOException {

    List<String> outputRecords = new ArrayList<>();
    try {
      outputRecords.addAll(dataMaskingCore.maskData(configuration, list, schemaType));
    } catch (IOException e) {
      log.logError(LogCodes.WPH6000E, e, "Unable to mask data");
      throw new DeidException("Unable to mask message", e);
    }
    return outputRecords;
  }

}
