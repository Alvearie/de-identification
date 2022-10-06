/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.metadata.model;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

/**
 * Container bean for metadata describing a configuration option for a masking provider.
 */
@JsonPropertyOrder({"name", "displayName", "description", "optionType", "default", "possibleValues",
    "excluded"})
public class ConfigurationOptionModel {

  /**
   * The data type of the value for the configuration option.
   */
  public static enum OptionType {
    String, RegEx, Boolean, Integer, Double, StringArray, RegExArray, MaskRuleSet, MaskingProvider
  }

  /**
   * the internal name for the configuration option
   */
  @JsonProperty("name")
  private String optionName;

  /**
   * a user-friendly (translated where possible) name for the configuration option
   */
  @JsonProperty("displayName")
  private String optionDisplayName;

  /**
   * the data type of the configuration option
   */
  private OptionType optionType;

  /**
   * a user-friendly description of the configuration option, translated where possible
   */
  @JsonProperty("description")
  private String optionDescription;

  /**
   * the default value for the configuration option, which can be <i>null</i>
   */
  @JsonProperty("default")
  private String optionDefault;

  /**
   * the possible values for the configuration option where the option supports only a finite set of
   * values
   */
  @JsonInclude(Include.NON_EMPTY)
  private List<ConfigurationOptionPossibleValueModel> possibleValues;

  /**
   * a list of values that are not allowed as the value of the configuration option, which might be
   * <i>null</i>
   */
  @JsonInclude(Include.NON_EMPTY)
  private List<String> excluded;

  public ConfigurationOptionModel() {
    // nothing required here
  }

  /**
   * 
   * @param nm the internal name of the configuration option
   * @param displayNm the user-friendly name of the configuration option
   * @param type the data type of the value of the configuration option
   * @param desc the user-friendly description of the configuration option
   * @param dflt the default value for the configuration option or <i>null</i> if no default is
   *        required or supported
   */
  public ConfigurationOptionModel(String nm, String displayNm, OptionType type, String desc,
      String dflt) {
    setOptionName(nm);
    setOptionDisplayName(displayNm);
    setOptionType(type);
    setOptionDescription(desc);
    setOptionDefault(dflt);
  }

  /**
   * @return the optionName
   */
  public String getOptionName() {
    return optionName;
  }

  /**
   * @param optionName the optionName to set
   */
  public void setOptionName(String optionName) {
    this.optionName = optionName;
  }

  /**
   * @return the optionDisplayName
   */
  public String getOptionDisplayName() {
    return optionDisplayName;
  }

  /**
   * @param optionDisplayName the optionDisplayName to set
   */
  public void setOptionDisplayName(String optionDisplayName) {
    this.optionDisplayName = optionDisplayName;
  }

  /**
   * @return the optionType
   */
  public OptionType getOptionType() {
    return optionType;
  }

  /**
   * @param optionType the optionType to set
   */
  public void setOptionType(OptionType optionType) {
    this.optionType = optionType;
  }

  /**
   * @return the optionDescription
   */
  public String getOptionDescription() {
    return optionDescription;
  }

  /**
   * @param optionDescription the optionDescription to set
   */
  public void setOptionDescription(String optionDescription) {
    this.optionDescription = optionDescription;
  }

  /**
   * @return the optionDefault
   */
  public String getOptionDefault() {
    return optionDefault;
  }

  /**
   * @param optionDefault the optionDefault to set
   */
  public void setOptionDefault(String optionDefault) {
    this.optionDefault = optionDefault;
  }

  /**
   * @return the possibleValues
   */
  public List<ConfigurationOptionPossibleValueModel> getPossibleValues() {
    return possibleValues;
  }

  /**
   * @param possibleValues the possibleValues to set
   */
  public void setPossibleValues(List<ConfigurationOptionPossibleValueModel> possibleValues) {
    this.possibleValues = possibleValues;
  }

  /**
   * @return the excluded
   */
  public List<String> getExcluded() {
    return excluded;
  }

  /**
   * @param excluded the excluded to set
   */
  public void setExcluded(List<String> excluded) {
    this.excluded = excluded;
  }
}
