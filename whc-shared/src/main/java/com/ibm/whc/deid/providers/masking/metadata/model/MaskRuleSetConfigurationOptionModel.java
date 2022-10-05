/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.metadata.model;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Container bean to describe a masking rule set that certain masking providers require to be
 * properly configured. Generally, this is a complex configuration option that can be repeated
 * multiple times in the configuration statements for the provider.
 */
public class MaskRuleSetConfigurationOptionModel extends ConfigurationOptionModel {

  /**
   * <i>True</i> if this portion of the masking provider configuration can be repeated and
   * <i>False</i> if not.
   */
  private boolean multiple = true;

  /**
   * The configuration option that can appear in each repetition of this seciton of configuration.
   */
  @JsonProperty("options")
  private List<? extends ConfigurationOptionModel> subOptions;

  public MaskRuleSetConfigurationOptionModel() {
    super("maskRuleSet", null, OptionType.MaskRuleSet, null, null);
  }

  @Override
  public OptionType getOptionType() {
    // is always this type
    return OptionType.MaskRuleSet;
  }

  @Override
  public void setOptionType(OptionType optionType) {
    // ignored - type cannot be changed
  }

  /**
   * @return whether multiples of this option can be configured
   */
  public boolean isMultiple() {
    return multiple;
  }

  /**
   * @param multiple whether multiples of this option can be configured
   */
  public void setMultiple(boolean multiple) {
    this.multiple = multiple;
  }

  /**
   * @return the configuration options for objects configured in this configuration option
   */
  public List<? extends ConfigurationOptionModel> getSubOptions() {
    return subOptions;
  }

  /**
   * @param subOptions the configuration options for objects configured in this configuration option
   */
  public void setSubOptions(List<? extends ConfigurationOptionModel> subOptions) {
    this.subOptions = subOptions;
  }
}
