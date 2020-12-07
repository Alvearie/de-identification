/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config;

import java.io.Serializable;
import java.util.List;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.ibm.whc.deid.shared.pojo.config.masking.MaskingProviderConfig;

/*
 * The rules set in the DeidMaskingConfig and used for masking data.
 */
public class Rule implements Serializable {

  private static final long serialVersionUID = -6263797438206665007L;
  
  public static final String NAME_PROPERTY_NAME = "name";
  public static final String PROVIDERS_PROPERTY_NAME = "maskingProviders";

  @JsonProperty(NAME_PROPERTY_NAME)
  private String name;

  @JsonProperty(PROVIDERS_PROPERTY_NAME)
  private List<MaskingProviderConfig> maskingProviders;

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public Rule() {}

  public Rule(String name, List<MaskingProviderConfig> maskingProviders) {
    super();
    this.name = name;
    this.maskingProviders = maskingProviders;
  }

  public List<MaskingProviderConfig> getMaskingProviders() {
    return maskingProviders;
  }

  public void setMaskingProviders(List<MaskingProviderConfig> maskingProviders) {
    this.maskingProviders = maskingProviders;
  }
}
