/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import java.util.List;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import com.ibm.whc.deid.providers.masking.fhir.MaskingActionInputIdentifier;
import com.ibm.whc.deid.shared.pojo.masking.ReferableData;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * Common base class for all classes providing privacy protection functions.
 */
public abstract class AbstractMaskingProvider implements MaskingProvider {

  private static final long serialVersionUID = -6276716005726979282L;

  protected SecureRandom random;
  protected boolean debug_enabled;

  protected final String localizationProperty;
  protected final String tenantId;

  protected LogManager log = LogManager.getInstance();

  private String name = "";

  public AbstractMaskingProvider() {
    this(null, null);
  }

  public AbstractMaskingProvider(String tenantId, String localizationProperty) {
    this.tenantId = tenantId;
    this.localizationProperty = localizationProperty;
  }

  @Override
  public String mask(String identifier, String fieldName) {
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
   * Sets to print only debug information
   *
   * @param value true or false
   */
  void setDebugEnabled(boolean value) {
    debug_enabled = value;
  }

  @Override
  public List<ReferableData> maskWithBatch(List<ReferableData> payloadData, String jobId) {
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

  protected final void putField(MaskingActionInputIdentifier i, String value) {
    if (i.getParent().isObject()) {
      TextNode newNode = new TextNode(value);
      i.setParent(((ObjectNode) i.getParent()).set(i.getPath(), newNode));
      i.setCurrentNode(newNode);
    } else if (i.getParent().isArray()) {
      ArrayNode aNode = (ArrayNode) i.getParent();
      int indexOfResult = -1;
      int size = aNode.size();
      for (int currentIndex = 0; currentIndex < size; currentIndex++) {
        if (aNode.get(currentIndex) == i.getNode()) {
          indexOfResult = currentIndex;
          break;
        }
      }
      TextNode newNode = new TextNode(value);
      aNode.set(indexOfResult, newNode);
      i.setCurrentNode(newNode);
    }
  }

  @Override
  public void setName(String name) {
    this.name = name;
  }

  @Override
  public String getName() {
    return name;
  }
}
