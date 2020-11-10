/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config;

/*
 * A class that contains the de-identification configuration for Factory classes
 */
public class DeidConfig {
  private String maskingProviderFactoryClass =
      "com.ibm.whc.deid.providers.masking.BasicMaskingProviderFactory";
  private String complexMaskingProviderFactoryClass =
      "com.ibm.whc.deid.providers.masking.ComplexMaskingProviderFactory";
  private String identifierFactoryClass =
      "com.ibm.whc.deid.providers.identifiers.BuiltInIdentifierFactory";
  private String objectMapperUtilClass = "com.ibm.whc.deid.ObjectMapperUtil";

  public String getMaskingProviderFactoryClass() {
    return maskingProviderFactoryClass;
  }

  public void setMaskingProviderFactoryClass(String maskingProviderFactoryClass) {
    this.maskingProviderFactoryClass = maskingProviderFactoryClass;
  }

  public String getComplexMaskingProviderFactoryClass() {
    return complexMaskingProviderFactoryClass;
  }

  public void setComplexMaskingProviderFactoryClass(String complexMaskingProviderFactoryClass) {
    this.complexMaskingProviderFactoryClass = complexMaskingProviderFactoryClass;
  }

  public String getIdentifierFactoryClass() {
    return identifierFactoryClass;
  }

  public String getObjectMapperUtilClass() {
    return objectMapperUtilClass;
  }

  public void setObjectMapperUtilClass(String objectMapperUtilClass) {
    this.objectMapperUtilClass = objectMapperUtilClass;
  }

}
