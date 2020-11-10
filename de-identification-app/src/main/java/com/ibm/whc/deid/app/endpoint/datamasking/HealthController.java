/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint.datamasking;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.endpoint.exception.BadRequestException;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/*
 * Health check rest endpoint - this class is used to tell if the rest endpoints are up
 */
@RestController
@RequestMapping("/api/v1")
@Api()
public class HealthController {
	static final LogManager log = LogManager.getInstance();

	@Autowired
	HealthController() {
	}

	@ApiOperation(value = "Get the status of the server", tags = { "Health" })
	@ApiResponses(value = {
			@ApiResponse(code = 200, message = "The masked output, as a JSON array.", response = String.class) })
	@GetMapping("/health")
	public ResponseEntity<?> getStatus()
			throws BadRequestException, DeidException {
		final ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
		ObjectNode node = objectMapper.createObjectNode();
		node.put("status", "UP");
		try {
			return new ResponseEntity<>(objectMapper.writeValueAsString(node), HttpStatus.OK);
		} catch (JsonProcessingException e) {
			log.logError(LogCodes.WPH6000E, e, "Unable to get status");
			throw new DeidException(e.getMessage());
		}
	}
}
