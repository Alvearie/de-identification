/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint.datamasking;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.endpoint.datamasking.AbstractDataMaskingInvoker;
import com.ibm.whc.deid.endpoint.exception.BadRequestException;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.exception.InvalidInputException;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaTypes;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.json.JsonMaskingRule;
import com.ibm.whc.deid.shared.pojo.masking.DataMaskingModel;
import com.ibm.whc.deid.shared.pojo.masking.DataMaskingObjectModel;
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
 * Data Masking rest endpoint - This class is the REST entry point for De-Identification requests.
 */
@RestController
@RequestMapping("/api/v1")
@Tag(name = "De-Identification", description = "Masking APIs")
public class DataMaskingController extends AbstractDataMaskingInvoker {

  private static final LogManager log = LogManager.getInstance();

  private final DataMaskingService dataMaskingService;

  @Autowired
  DataMaskingController(DataMaskingService dataMaskingService) {
    this.dataMaskingService = dataMaskingService;
  }

  @Operation(summary = "de-identify single data",
      description = "De-identify the given JSON message using the given configuration",
      tags = {"De-Identification"})
  @ApiResponses(value = {@ApiResponse(responseCode = "200",
      description = "The masked output in JSON object format",
      content = @Content(schema = @Schema(implementation = DataMaskingObjectModel.class)))})
  @PostMapping(path = "/deidentification/single")
  public ResponseEntity<?> maskSingleJson(@RequestBody DataMaskingObjectModel maskingRequest)
      throws BadRequestException, DeidException, InvalidInputException {
    try {
      DeidMaskingConfig config = maskingRequest.getConfig();
      if (config == null) {
        throw new InvalidInputException("no masking configuration data");
      }
      // remove rule assignments that do not name a rule
      if (config.getJson() != null && config.getJson().getMaskingRules() != null) {
        config.getJson().setMaskingRules(removeNullRules(config.getJson().getMaskingRules()));
      }
      MaskingConfigUtils.getInstance().validateConfigObject(config);

      JsonNode data = maskingRequest.getData();
      if (data == null || data.isNull()) {
        throw new InvalidInputException("no input to de-identify");
      }
      if (data.isArray()) {
        throw new InvalidInputException("invalid input data format - array not supported");
      }
      if (!data.isObject()) {
        throw new InvalidInputException("invalid input data format - data must be JSON object");
      }
      String dataString = ObjectMapperFactory.getObjectMapper().writeValueAsString(data);
      List<String> dataList = new ArrayList<>(1);
      dataList.add(dataString);

      return maskJsonMethod(dataList, maskingRequest.getSchemaType(), config, dataMaskingService);

    } catch (JsonProcessingException e) {
      throw new BadRequestException(e.getMessage());
    } catch (InvalidMaskingConfigurationException e) {
      throw new BadRequestException(e.getMessage());
    }
  }

  @Operation(summary = "de-identify data",
      description = "De-identify the given messages using the given configuration",
      tags = {"De-Identification"})
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "The protected output as a JSON array.",
          content = @Content(schema = @Schema(implementation = String.class)))})
  @PostMapping("/deidentification")
  public ResponseEntity<?> maskJson(@RequestBody DataMaskingModel maskRequest)
      throws BadRequestException, DeidException, InvalidInputException {
    try {
      validateData(maskRequest.getData());
      return maskJsonMethod(maskRequest.getData(), maskRequest.getSchemaType(),
          MaskingConfigUtils.validateConfig(maskRequest.getConfig()), dataMaskingService);
    } catch (InvalidMaskingConfigurationException e) {
      throw new BadRequestException(e.getMessage());
    }
  }

  protected ResponseEntity<?> maskJsonMethod(List<String> data, ConfigSchemaTypes schemaType,
      DeidMaskingConfig maskingConfig, DataMaskingService service)
      throws DeidException, InvalidInputException {
    List<String> maskedData;
    try {
      validateSchemaType(schemaType);
      // masking config is already validated

      maskedData = service.maskData(maskingConfig, data, schemaType);

      ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
      String maskedOutput = getString(objectMapper, maskedData);

      return new ResponseEntity<>(maskedOutput, HttpStatus.OK);
    } catch (IOException e) {
      log.logError(LogCodes.WPH6000E, e, "Unable to mask data");
      throw new DeidException(e.getMessage());
    }
  }

  protected List<JsonMaskingRule> removeNullRules(List<JsonMaskingRule> maskingRules) {
    return maskingRules.parallelStream().filter(rule -> rule != null && rule.getRule() != null)
        .collect(Collectors.toList());
  }
}
