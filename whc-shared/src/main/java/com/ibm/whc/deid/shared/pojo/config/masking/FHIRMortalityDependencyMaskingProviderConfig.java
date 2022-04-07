/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/**
 * Configuration for FHIRMortalityDependencyMaskingProvider.
 */
@JsonInclude(Include.NON_NULL)
public class FHIRMortalityDependencyMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -4162093284769724219L;

  /**
   * The minimum age of the patient in number of years before the privacy provider will allow
   * mortality indicators to remain in the data.
   */
  private int mortalityIndicatorMinYears = 8;

  public FHIRMortalityDependencyMaskingProviderConfig() {
    type = MaskingProviderType.FHIR_MORTALITY_DEPENDENCY;
  }

  public int getMortalityIndicatorMinYears() {
    return mortalityIndicatorMinYears;
  }

  public void setMortalityIndicatorMinYears(int mortalityIndicatorMinYears) {
    this.mortalityIndicatorMinYears = mortalityIndicatorMinYears;
  }

  @Override
  public void validate(DeidMaskingConfig maskingConfig)
      throws InvalidMaskingConfigurationException {
    super.validate(maskingConfig);
    if (mortalityIndicatorMinYears < 0) {
      throw new InvalidMaskingConfigurationException(
          "`mortalityIndicatorMinYears` must be greater than or equal to 0");
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + Objects.hash(mortalityIndicatorMinYears);
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (!super.equals(obj)) {
      return false;
    }
    if (!(obj instanceof FHIRMortalityDependencyMaskingProviderConfig)) {
      return false;
    }
    FHIRMortalityDependencyMaskingProviderConfig other =
        (FHIRMortalityDependencyMaskingProviderConfig) obj;
    return mortalityIndicatorMinYears == other.mortalityIndicatorMinYears;
  }
}
