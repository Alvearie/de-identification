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
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

/*
 * Health check rest endpoint - this class is used to tell if the rest endpoints are up
 */
@RestController
@RequestMapping("/api/v1")
@Tag(name = "Health", description = "Health apis")
public class HealthController {
	static final LogManager log = LogManager.getInstance();

	@Autowired
	HealthController() {
	}

    @Operation(summary = "server status", description = "Get the status of the server",
        tags = {"Health"})
	@ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "The masked output, as a JSON array.",
            content = @Content(schema = @Schema(implementation = String.class)))})
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
