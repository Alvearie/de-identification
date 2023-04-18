/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.json;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;

/*
 * JsonConfig object used in DeidMaskingConfig
 */
@JsonInclude(Include.NON_NULL)
public class JsonConfig implements Serializable {

  private static final long serialVersionUID = -3944211778772667702L;

  public static final String SCHEMA_TYPE_PROPERTY_NAME = "schemaType";  
  public static final String MESSAGE_TYPE_KEY_PROPERTY_NAME = "messageTypeKey";
  public static final String MESSAGE_TYPES_PROPERTY_NAME = "messageTypes";
  public static final String RULES_PROPERTY_NAME = "maskingRules";
  
  @JsonProperty(SCHEMA_TYPE_PROPERTY_NAME)
  private ConfigSchemaType schemaType;
  @JsonProperty(MESSAGE_TYPE_KEY_PROPERTY_NAME)
  private String messageTypeKey; // default to null
  @JsonProperty(MESSAGE_TYPES_PROPERTY_NAME)
  private List<String> messageTypes;
  @JsonProperty(RULES_PROPERTY_NAME)
  private List<JsonMaskingRule> maskingRules = new ArrayList<>();

  public ConfigSchemaType getSchemaType() {
    return schemaType;
  }

  public void setSchemaType(ConfigSchemaType schemaType) {
    this.schemaType = schemaType;
  }

  public String getMessageTypeKey() {
    return messageTypeKey;
  }

  public void setMessageTypeKey(String messageTypeKey) {
    this.messageTypeKey = messageTypeKey;
  }

  public List<String> getMessageTypes() {
    return messageTypes;
  }

  public void setMessageTypes(List<String> messageTypes) {
    this.messageTypes = messageTypes;
  }

  public List<JsonMaskingRule> getMaskingRules() {
    return maskingRules;
  }

  public void setMaskingRules(List<JsonMaskingRule> maskingRules) {
    this.maskingRules = maskingRules;
  }

  public void addMaskingRule(String jsonPath, String rule) {
    JsonMaskingRule maskingRule = new JsonMaskingRule();
    maskingRule.setJsonPath(jsonPath);
    maskingRule.setRule(rule);
    if (maskingRules == null) {
      maskingRules = new ArrayList<>();
    }
    maskingRules.add(maskingRule);
  }
}
