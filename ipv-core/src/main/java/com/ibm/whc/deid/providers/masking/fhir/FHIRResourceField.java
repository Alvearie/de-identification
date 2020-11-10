/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

public class FHIRResourceField {
  private final String shortRuleName;
  private final String identifierPath;

  public String getKey() {
    return identifierPath;
  }

  public String getShortRuleName() {
    return shortRuleName;
  }

  public FHIRResourceField(String identifierPath, String shortRuleName) {
    this.identifierPath = identifierPath;
    this.shortRuleName = shortRuleName;
  }
}
