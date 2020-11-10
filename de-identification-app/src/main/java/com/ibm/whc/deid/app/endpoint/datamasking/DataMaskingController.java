/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint.datamasking;

import java.io.IOException;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.endpoint.datamasking.AbstractDataMaskingInvoker;
import com.ibm.whc.deid.endpoint.exception.BadRequestException;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.exception.InvalidInputException;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;
import com.ibm.whc.deid.shared.pojo.masking.DataMaskingModel;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;
import com.ibm.whc.deid.shared.util.MaskingConfigUtils;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/*
 * Data Masking rest endpoint - this class is the entry point for deidentification requests
 */
@RestController
@RequestMapping("/api/v1")
@Api()
public class DataMaskingController extends AbstractDataMaskingInvoker {

  private final DataMaskingService dataMaskingService;
  private static final LogManager log = LogManager.getInstance();

  @Autowired
  DataMaskingController(DataMaskingService dataMaskingService) {
    this.dataMaskingService = dataMaskingService;
  }

  @ApiOperation(value = "Deidentify data using the configuration provided in the tenantConfigs API",
      tags = {"De-Identification"})
  @ApiResponses(value = {@ApiResponse(code = 200, message = "The masked output, as a JSON array.",
      response = String.class)})
  @PostMapping("/deidentification")
  public ResponseEntity<?> maskJson(@RequestBody DataMaskingModel maskRequest)
      throws BadRequestException, DeidException, InvalidInputException {
    try {
      return maskJsonMethod(maskRequest, dataMaskingService);
    } catch (InvalidMaskingConfigurationException e) {
      throw new BadRequestException(e.getMessage());
    }
  }

  protected ResponseEntity<?> maskJsonMethod(DataMaskingModel maskRequest,
      DataMaskingService dataMaskingService)
      throws InvalidMaskingConfigurationException, DeidException, InvalidInputException {
    List<String> maskedData;
    try {
      String config = maskRequest.getConfig();
      List<String> data = maskRequest.getData();
      ConfigSchemaTypes schemaType = maskRequest.getSchemaType();
      MaskingConfigUtils.validateConfig(maskRequest.getConfig());
      validateData(data);
      validateSchemaType(schemaType);
      maskedData = dataMaskingService.maskData(config, data, schemaType);

      ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
      String maskedOutput = getString(objectMapper, maskedData);

      return new ResponseEntity<>(maskedOutput, HttpStatus.OK);
    } catch (IOException e) {
      log.logError(LogCodes.WPH6000E, e, "Unable to mask data");
      throw new DeidException(e.getMessage());
    }
  }

}
