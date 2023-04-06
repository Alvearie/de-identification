/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.masking;

/*
 * The model class which holds a reference string identifier and some data
 */
public class ReferableData {

  private String identifier;
  private String data;

  public ReferableData(String data) {
    this.identifier = null;
    this.data = data;
  }

  public ReferableData(String identifier, String data) {
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
