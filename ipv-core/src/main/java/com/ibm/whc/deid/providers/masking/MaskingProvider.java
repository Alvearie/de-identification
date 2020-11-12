/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import com.ibm.whc.deid.models.OriginalMaskedValuePair;
import com.ibm.whc.deid.providers.masking.fhir.MaskingActionInputIdentifier;
import com.ibm.whc.deid.schema.FieldRelationship;
import com.ibm.whc.deid.shared.pojo.masking.IdentifiedData;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * The interface Masking provider.
 *
 */
public interface MaskingProvider extends Serializable {
  /**
   * Mask string [ ].
   *
   * @param data the data
   * @return the string [ ]
   */
  String[] mask(final String[] data);

  /**
   * Mask string.
   *
   * @param identifier the identifier
   * @return the string
   */
  String mask(String identifier);

  /**
   * Mask byte [ ].
   *
   * @param data the data
   * @return the byte [ ]
   */
  byte[] mask(byte[] data);

  /**
   * Mask string.
   *
   * @param identifier the identifier
   * @param fieldName the field name
   * @return the string
   */
  String mask(String identifier, String fieldName);

  /**
   * Mask string.
   *
   * @param identifier the identifier
   * @param fieldName the field name
   * @param fieldRelationship the field relationship
   * @param values the values
   * @return the string
   */
  String mask(String identifier, String fieldName, FieldRelationship fieldRelationship,
      Map<String, OriginalMaskedValuePair> values);

  void setTestingOnlyLogManager(LogManager logManager);

  public void maskIdentifierBatch(List<MaskingActionInputIdentifier> identifiers);

  public List<IdentifiedData> maskWithBatch(List<IdentifiedData> payloadData,
      String jobId);

  void setName(String ruleName);

  String getName();
}
