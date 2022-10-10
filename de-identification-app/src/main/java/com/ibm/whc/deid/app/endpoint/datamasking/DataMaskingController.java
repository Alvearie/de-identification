/*
 * © Merative US L.P. 2016, 2022
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
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.endpoint.datamasking.AbstractDataMaskingInvoker;
import com.ibm.whc.deid.endpoint.exception.BadRequestException;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.exception.InvalidInputException;
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
      throws BadRequestException, InvalidInputException {
    try {
      ObjectNode data = maskingRequest.getData();
      if (data == null || data.size() == 0) {
        throw new InvalidInputException("no input to de-identify");
      }
      String dataString = ObjectMapperFactory.getObjectMapper().writeValueAsString(data);
      List<String> dataList = new ArrayList<>(1);
      dataList.add(dataString);

      DeidMaskingConfig config = maskingRequest.getConfig();
      // remove rule assignments that do not name a rule
      if (config != null && config.getJson() != null
          && config.getJson().getMaskingRules() != null) {
        config.getJson().setMaskingRules(removeNullRules(config.getJson().getMaskingRules()));
      }
      MaskingConfigUtils.getInstance().validateConfigObject(config);

      validateSchemaType(maskingRequest.getSchemaType());

      List<String> maskedData =
          dataMaskingService.maskData(config, dataList, maskingRequest.getSchemaType());

      String maskedOutput = null;
      if (!maskedData.isEmpty()) {
        ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();
        ObjectNode maskedNode = (ObjectNode) mapper.readTree(maskedData.get(0));
        ObjectNode outputNode = mapper.createObjectNode();
        outputNode.set("data", maskedNode);
        maskedOutput = mapper.writeValueAsString(outputNode);
      }
      return new ResponseEntity<>(maskedOutput, HttpStatus.OK);

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
      DeidMaskingConfig maskingConfig = MaskingConfigUtils.validateConfig(maskRequest.getConfig());
      validateSchemaType(maskRequest.getSchemaType());

      List<String> maskedData = dataMaskingService.maskData(maskingConfig, maskRequest.getData(),
          maskRequest.getSchemaType());
      String maskedOutput = getString(ObjectMapperFactory.getObjectMapper(), maskedData);

      return new ResponseEntity<>(maskedOutput, HttpStatus.OK);

    } catch (IOException e) {
      log.logError(LogCodes.WPH6000E, e, "Unable to mask data");
      throw new DeidException(e.getMessage());
    } catch (InvalidMaskingConfigurationException e) {
      throw new BadRequestException(e.getMessage());
    }
  }

  protected List<JsonMaskingRule> removeNullRules(List<JsonMaskingRule> maskingRules) {
    return maskingRules.parallelStream().filter(rule -> rule != null && rule.getRule() != null)
        .collect(Collectors.toList());
  }
}
