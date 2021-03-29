/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import java.util.ArrayList;
import java.util.HashMap;
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

  public FHIRResourceMaskingConfiguration(String basePath, List<FHIRResourceField> fields) {
    this.basePath = basePath;

    if (fields == null || fields.isEmpty()) {
      this.fields = new ArrayList<>();
    } else {
      // If multiple rule assignments are made for same path, validation will have already
      // ensured all such rule assignments specified the same rule. Ensure the rule is only
      // applied to the path once.
      ArrayList<FHIRResourceField> tempList = new ArrayList<>(fields.size());
      HashMap<String, Integer> previouslyAddedMap = new HashMap<>(fields.size() * 2);
      for (FHIRResourceField field : fields) {
        Integer currentIndex = Integer.valueOf(tempList.size());
        tempList.add(field);

        Integer previousOffset = previouslyAddedMap.put(field.getKey(), currentIndex);
        if (previousOffset != null) {
          tempList.set(previousOffset.intValue(), null);
        }
      }
      ArrayList<FHIRResourceField> keepList = new ArrayList<>(fields.size());
      for (FHIRResourceField field : tempList) {
        if (field != null) {
          keepList.add(field);
        }
      }
      this.fields = keepList;
    }
  }

}
