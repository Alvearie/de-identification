/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

public class IdentifiedData {

  private String identifier;
  private String data;

  public IdentifiedData(String data) {
    this.identifier = null;
    this.data = data;
  }

  public IdentifiedData(String identifier, String data) {
    this.identifier = identifier;
    this.data = data;
  }

  public String getIdentifier() {
    return identifier;
  }

  public void setIdentifier(String identifier) {
    this.identifier = identifier;
  }

  public String getData() {
    return data;
  }

  public void setData(String data) {
    this.data = data;
  }

}
