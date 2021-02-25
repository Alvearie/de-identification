/*
 * (C) Copyright IBM Corp. 2016,2021
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
import com.ibm.whc.deid.shared.pojo.masking.ReferableData;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * Interface supported by classes that provide privacy protection functions.
 */
public interface MaskingProvider extends Serializable {

  /**
   * Applies a privacy protection operation to a string.
   *
   * @param identifier the input data to be protected
   * 
   * @return the protected value to use
   */
  public String mask(String identifier);

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

  public void maskIdentifierBatch(List<MaskingActionInputIdentifier> identifiers);

  public List<ReferableData> maskWithBatch(List<ReferableData> payloadData, String jobId);

  public void setName(String ruleName);

  public String getName();
}
