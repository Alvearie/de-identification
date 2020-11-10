/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.io.Serializable;
import java.security.SecureRandom;
import java.util.List;
import java.util.Map;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import com.ibm.whc.deid.models.OriginalMaskedValuePair;
import com.ibm.whc.deid.providers.masking.fhir.MaskingActionInputIdentifier;
import com.ibm.whc.deid.schema.FieldRelationship;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import scala.Tuple2;

/**
 * The type Abstract masking provider.
 *
 */
public abstract class AbstractMaskingProvider implements MaskingProvider, Serializable {
  /** */
  private static final long serialVersionUID = -6276716005726979282L;

  protected SecureRandom random;
  protected LogManager testingOnly_LogManager;
  protected boolean debug_enabled;

  protected LogManager log = LogManager.getInstance();

  private String name = "";

  @Override
  public String[] mask(String[] data) {
    final String[] maskedData = new String[data.length];

    for (int i = 0; i < data.length; ++i)
      maskedData[i] = mask(data[i]);

    return maskedData;
  }

  @Override
  public String mask(String identifier, String fieldName) {
    return mask(identifier);
  }

  @Override
  public byte[] mask(byte[] data) {
    return mask(new String(data)).getBytes();
  }

  @Override
  public String mask(String identifier, String fieldName, FieldRelationship fieldRelationship,
      Map<String, OriginalMaskedValuePair> maskedValues) {
    return mask(identifier);
  }

  protected void debugFaultyInput(String faultyInput) {
    if (log.isDebugEnabled()) {
      log.logDebug(LogCodes.WPH1015D, faultyInput, this.getClass().getName());
    }
  }

  protected void warnFaultyInput(String faultyInput) {
    if (log.isWarnEnabled()) {
      log.logWarn(LogCodes.WPH1011W, " field " + faultyInput, this.getClass().getName());
    }
  }

  /**
   * Logs information about an exception to the application audit log.
   *
   * @param e the exception
   */
  protected void logException(Exception e) {
    if (log.isErrorEnabled()) {
      log.logError(LogCodes.WPH2004E, e, e.getClass().getName() + " processing field ");
    }
  }

  /**
   * For testing only.
   *
   * @param logManager the log manager
   */
  public void setTestingOnlyLogManager(LogManager logManager) {
    testingOnly_LogManager = logManager;
  }

  /**
   * Sets to print only debug information
   *
   * @param value true or false
   */
  void setDebugEnabled(boolean value) {
    debug_enabled = value;
  }

  @Override
  public List<Tuple2<String, String>> maskWithBatch(List<Tuple2<String, String>> payloadData,
      String jobId) {
    return null;
  }

  @Override
  public void maskIdentifierBatch(List<MaskingActionInputIdentifier> identifiers) {
    for (MaskingActionInputIdentifier i : identifiers) {
      String value = i.getNode().asText();
      value = mask(value);
      putField(i, value);
    }
  }

  protected void putField(MaskingActionInputIdentifier i, String value) {
    if (i.getParent().isObject()) {
      i.setParent(((ObjectNode) i.getParent()).set(i.getPath(), new TextNode(value)));
    } else if (i.getParent().isArray()) {
      ArrayNode aNode = (ArrayNode) i.getParent();
      int indexOfResult = -1;
      for (int currentIndex = 0; currentIndex < aNode.size(); currentIndex++) {
        if (aNode.get(currentIndex).equals(i.getNode())) {
          indexOfResult = currentIndex;
          break;
        }
      }
      aNode.set(indexOfResult, new TextNode(value));
    }
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }
}
