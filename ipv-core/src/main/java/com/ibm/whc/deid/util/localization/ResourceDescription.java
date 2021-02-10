/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util.localization;

import java.util.Collection;
import com.ibm.whc.deid.shared.localization.Resources;

public class ResourceDescription {
  private final Resources resource;
  private final ResourceFormat format;
  private final String description;
  private Collection<String> fieldDescriptions;

  /**
   * Gets resource.
   *
   * @return the resource
   */
  public Resources getResource() {
    return resource;
  }

  /**
   * Gets format.
   *
   * @return the format
   */
  public ResourceFormat getFormat() {
    return format;
  }

  /**
   * Gets description.
   *
   * @return the description
   */
  public String getDescription() {
    return description;
  }

  /**
   * Gets field descriptions.
   *
   * @return the field descriptions
   */
  public Collection<String> getFieldDescriptions() {
    return fieldDescriptions;
  }

  /**
   * Instantiates a new Resource description.
   *
   * @param resource the resource
   * @param format the format
   * @param formatOptions the format options
   * @param description the description
   * @param fieldDescriptions the field descriptions
   */
  public ResourceDescription(Resources resource, ResourceFormat format,
      Collection<String> formatOptions, String description, Collection<String> fieldDescriptions) {
    this.resource = resource;
    this.format = format;
    this.description = description;
    this.fieldDescriptions = fieldDescriptions;
  }
}
