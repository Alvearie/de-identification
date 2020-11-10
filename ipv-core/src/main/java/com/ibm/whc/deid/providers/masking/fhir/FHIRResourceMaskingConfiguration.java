/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

public class FHIRResourceMaskingConfiguration {
  private final String basePath;
  private final List<FHIRResourceField> fields;

  public String getBasePath() {
    return basePath;
  }

  public List<FHIRResourceField> getFields() {
    return fields;
  }

  /**
   * Reads masking configuration to retrieve the type of masking providers
   *
   * @param conf
   * @return
   */
  private FHIRResourceField buildResourceField(Entry<String, String> confLine) {
    String path = confLine.getValue();
    String key = confLine.getKey();
    return new FHIRResourceField(key, path);
  }

  /**
   * Reads masking to parse the resources and sets them
   *
   * @param configurations
   * @return
   */
  private List<FHIRResourceField> buildFieldList(Map<String, String> configurations) {
    List<FHIRResourceField> resourceFields = new ArrayList<>(configurations.size());

    for (Entry<String, String> conf : configurations.entrySet()) {
      FHIRResourceField resourceField = buildResourceField(conf);
      resourceFields.add(resourceField);
    }

    return resourceFields;
  }

  public FHIRResourceMaskingConfiguration(String basePath, Map<String, String> configurations) {
    this.basePath = basePath;
    this.fields = buildFieldList(configurations);
  }
}
