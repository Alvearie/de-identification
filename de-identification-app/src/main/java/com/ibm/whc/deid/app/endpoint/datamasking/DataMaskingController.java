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
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

/*
 * Data Masking rest endpoint - this class is the entry point for deidentification requests
 */
@RestController
@RequestMapping("/api/v1")
@Tag(name = "De-Identification", description = "Masking apis")
public class DataMaskingController extends AbstractDataMaskingInvoker {

  private final DataMaskingService dataMaskingService;
  private static final LogManager log = LogManager.getInstance();

  @Autowired
  DataMaskingController(DataMaskingService dataMaskingService) {
    this.dataMaskingService = dataMaskingService;
  }

  @Operation(summary = "deidentify data",
      description = "Deidentify data using the configuration provided in the tenantConfigs API",
      tags = {"De-Identification"})
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "The masked output, as a JSON array.",
          content = @Content(schema = @Schema(implementation = String.class)))})
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
