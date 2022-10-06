/*
 * (C) Copyright Merative 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint.metadata;

import java.util.Locale;
import javax.servlet.http.HttpServletRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import com.ibm.whc.deid.providers.masking.metadata.model.ProvidersMetadata;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

/**
 * REST API endpoints for obtaining descriptive information about supported privacy providers.
 */
@RestController
@RequestMapping("/api/v1")
@Tag(name = "Metadata", description = "Information APIs")
public class MaskingProvidersMetadataController {

  private MaskingProvidersMetadataService service;

  @Autowired
  public MaskingProvidersMetadataController(MaskingProvidersMetadataService service) {
    this.service = service;
  }

  /**
   * Get the metadata describing all of the privacy providers.
   *
   * @return A metadata object with information about each of the available masking providers
   */
  @Operation(
      description = "Get information about the configuration options for the privacy providers.",
      summary = "privacy providers information", tags = {"Metadata"})
  @ApiResponses(
      value = {@ApiResponse(responseCode = "200", description = "providers metadata response",
      content = @Content(schema = @Schema(implementation = ProvidersMetadata.class)))})
  @RequestMapping(value = "/maskingproviders", method = RequestMethod.GET)
  public ResponseEntity<ProvidersMetadata> getMaskingProviderMetadata(HttpServletRequest request) {

    Locale locale = request.getLocale(); // does not return null

    ProvidersMetadata md = service.getMaskingProvidersMetadata(locale);

    return new ResponseEntity<>(md, HttpStatus.OK);
  }
}
