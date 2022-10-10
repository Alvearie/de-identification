/*
 * © Merative US L.P. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.metadata.model;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

/**
 * Container bean for information about a group of masking provider configuration options.
 */
@JsonPropertyOrder({"name", "displayName", "options"})
public class ConfigurationOptionGroupModel {

  @JsonProperty("name")
  private String groupName;
  @JsonProperty("displayName")
  private String groupDisplayName;
  @JsonProperty("options")
  private List<ConfigurationOptionModel> groupOptions;

  public ConfigurationOptionGroupModel() {
    // instantiate with null members
  }

  public ConfigurationOptionGroupModel(String name, String display) {
    groupName = name;
    groupDisplayName = display;
  }

  /**
   * @return the groupName
   */
  public String getGroupName() {
    return groupName;
  }

  /**
   * @param groupName the groupName to set
   */
  public void setGroupName(String groupName) {
    this.groupName = groupName;
  }

  /**
   * @return the groupDisplayName
   */
  public String getGroupDisplayName() {
    return groupDisplayName;
  }

  /**
   * @param groupDisplayName the groupDisplayName to set
   */
  public void setGroupDisplayName(String groupDisplayName) {
    this.groupDisplayName = groupDisplayName;
  }

  /**
   * @return the groupOptions
   */
  public List<ConfigurationOptionModel> getGroupOptions() {
    return groupOptions;
  }

  /**
   * @param groupOptions the groupOptions to set
   */
  public void setGroupOptions(List<ConfigurationOptionModel> groupOptions) {
    this.groupOptions = groupOptions;
  }
}
