/*
 * (C) Copyright IBM Corp. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.metadata.model;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType.MaskingProviderCategory;

/**
 * Container for metadata that describes a masking provider and its configuration options.
 */
@JsonPropertyOrder({"name", "description", "category", "docLink", "rulePreview", "options",
    "optionGroups"})
public class MaskingProviderMetadataModel {

  /**
   * the internal name of the masking provider
   */
  private String name;

  /**
   * a user-friendly description of the masking provider, translated if/where possible
   */
  private String description;

  /**
   * the category of the masking provider, which indicates the type of data it was designed to mask
   * and how it can participate in rule chaining
   */
  private MaskingProviderCategory category;

  /**
   * a URL for online detailed documentation about this masking provider
   */
  private String docLink;

  /**
   * <i>True</i> if the user interface should support a feature to test the configuration of this
   * masking provider with user-supplied data and <i>False</i> if not
   */
  private boolean rulePreview = true;

  /**
   * A list of the configuration options for this masking provider in the order they should be
   * presented to a user.
   */
  @JsonInclude(Include.NON_EMPTY)
  private List<ConfigurationOptionModel> options;

  /**
   * A list of related configuration options for this masking provider in the order they should be
   * presented to a user. This allows related options to be presented as part of a logical group.
   * The groups are listed in the order they should be presented to a user.
   */
  @JsonInclude(Include.NON_EMPTY)
  private List<ConfigurationOptionGroupModel> optionGroups;


  /**
   * @return the name
   */
  public String getName() {
    return name;
  }

  /**
   * @param name the name to set
   */
  public void setName(String name) {
    this.name = name;
  }

  /**
   * @return the description
   */
  public String getDescription() {
    return description;
  }

  /**
   * @param description the description to set
   */
  public void setDescription(String description) {
    this.description = description;
  }

  /**
   * @return the category
   */
  public MaskingProviderCategory getCategory() {
    return category;
  }

  /**
   * @param category the category to set
   */
  public void setCategory(MaskingProviderCategory category) {
    this.category = category;
  }

  /**
   * @return the docLink
   */
  public String getDocLink() {
    return docLink;
  }

  /**
   * @param docLink the docLink to set
   */
  public void setDocLink(String docLink) {
    this.docLink = docLink;
  }

  /**
   * @return the rulePreview
   */
  public boolean isRulePreview() {
    return rulePreview;
  }

  /**
   * @param rulePreview the rulePreview to set
   */
  public void setRulePreview(boolean rulePreview) {
    this.rulePreview = rulePreview;
  }

  /**
   * @return the options
   */
  public List<ConfigurationOptionModel> getOptions() {
    return options;
  }

  /**
   * @param options the options to set
   */
  public void setOptions(List<ConfigurationOptionModel> options) {
    this.options = options;
  }

  /**
   * @return the optionGroups
   */
  public List<ConfigurationOptionGroupModel> getOptionGroups() {
    return optionGroups;
  }

  /**
   * @param optionGroups the optionGroups to set
   */
  public void setOptionGroups(List<ConfigurationOptionGroupModel> optionGroups) {
    this.optionGroups = optionGroups;
  }
}
