/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * This class is used as a singleton to stand up an objectMapper.
 */
public class FHIRMaskingUtils {
  private static ObjectMapper objectMapper = new ObjectMapper();


  /**
   * This is what the application needs to stand up as a singleton
   *
   * @return
   */
  public static ObjectMapper getObjectMapper() {
    return objectMapper;
  }
}
