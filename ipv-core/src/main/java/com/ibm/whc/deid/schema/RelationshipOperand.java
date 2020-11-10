/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.schema;

import java.io.Serializable;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.ibm.whc.deid.providers.ProviderType;

public class RelationshipOperand implements Serializable {
  /** */
  private static final long serialVersionUID = -7094280647767423497L;

  private String name;
  private ProviderType type;

  /**
   * Instantiates a new Relationship operand.
   *
   * @param name the name
   * @param type the type
   */
  @JsonCreator
  public RelationshipOperand(@JsonProperty("name") String name,
      @JsonProperty("type") ProviderType type) {
    this.name = name;
    this.type = type;
  }

  /**
   * Instantiates a new Relationship operand.
   *
   * @param name the name
   */
  public RelationshipOperand(String name) {
    this(name, null);
  }

  /**
   * Gets name.
   *
   * @return the name
   */
  public String getName() {
    return this.name;
  }

  /**
   * Gets type.
   *
   * @return the type
   */
  public ProviderType getType() {
    return this.type;
  }

  public String toString() {
    if (this.type != null)
      return this.name + ":" + this.type.getName();
    return this.name + ":null";
  }
}
