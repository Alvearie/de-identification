/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import java.util.List;
import java.util.function.Supplier;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import com.ibm.whc.deid.providers.masking.fhir.MaskingActionInputIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

/**
 * Common base class for all classes providing privacy protection functions.
 */
public abstract class AbstractMaskingProvider implements MaskingProvider {

  private static final long serialVersionUID = -6276716005726979282L;

  protected static final LogManager log = LogManager.getInstance();

  protected SecureRandom random;
  protected boolean debug_enabled;

  protected final String localizationProperty;
  protected final String tenantId;

  protected final UnexpectedMaskingInputHandler unexpectedInputHandler;
  protected final String unexpectedInputReturnMessage;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  private String name = "";

  public AbstractMaskingProvider() {
    this(null, null, null);
  }

  public AbstractMaskingProvider(MaskingProviderConfig config) {
    this(null, null, config);
  }

  public AbstractMaskingProvider(String tenantId, String localizationProperty) {
    this(tenantId, localizationProperty, null);
  }

  public AbstractMaskingProvider(String tenantId, String localizationProperty,
      MaskingProviderConfig config) {
    this.tenantId = tenantId;
    this.localizationProperty = localizationProperty;
    this.unexpectedInputHandler = config == null ? null : config.getUnexpectedInputHandling();
    this.unexpectedInputReturnMessage =
        config == null ? null : config.getUnexpectedInputReturnMessage();
    this.unspecifiedValueHandling = config == null ? 1 : config.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage =
        config == null ? null : config.getUnspecifiedValueReturnMessage();
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
  public void maskIdentifierBatch(List<MaskingActionInputIdentifier> identifiers) {
    for (MaskingActionInputIdentifier i : identifiers) {
      String value = i.getNode().asText();
      value = mask(value);
      putField(i, value);
    }
  }

  protected final void putField(MaskingActionInputIdentifier i, String value) {
    // TextNode does not expect null as its value, so use NullNode.
    // TextNode typically works, but as of Jackson 2.11.4, some methods like hashCode() get NPE.
    JsonNode newNode = value == null ? NullNode.getInstance() : new TextNode(value);
    if (i.getParent().isObject()) {
      ((ObjectNode) i.getParent()).set(i.getPath(), newNode);
      i.setCurrentNode(newNode);
    } else if (i.getParent().isArray()) {
      // Do not use object identity to find the array member.
      // Jackson flyweights certain node objects so the same JsonNode
      // can appear multiple times within a document tree.
      String path = i.getPath();
      int index = path.lastIndexOf('[');
      if (index < 0) { // safety check - should not occur
        throw new RuntimeException("path to member of array node does not contain `[`");
      }
      int closeIndex = path.indexOf(']', index);
      if (closeIndex < 0) { // safety check - should not occur
        throw new RuntimeException("path to member of array node does not contain `]` after `[`");
      }
      int offset;
      try {
        offset = Integer.parseInt(path.substring(index + 1, closeIndex));
      } catch (NumberFormatException e) {
        // should not occur
        throw new RuntimeException("offset in path cannot be converted to an integer");
      }
      try {
        ((ArrayNode) i.getParent()).set(offset, newNode);
      } catch (IndexOutOfBoundsException e) {
        // should not occur
        throw new RuntimeException("offset in path was " + offset + " but ArrayNode has only "
            + ((ArrayNode) i.getParent()).size() + " members");
      }
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

  protected String applyUnexpectedValueHandling(String input, Supplier<String> randomGenerator) {
    // do not put the actual input value into the log - it could contain PHI
    debugFaultyInput("input");
    String response;

    if (unexpectedInputHandler != null) {
      switch (unexpectedInputHandler) {
        case NULL:
          response = null;
          break;
        case RANDOM:
          response = randomGenerator == null ? null : randomGenerator.get();
          break;
        case MESSAGE:
          response = unexpectedInputReturnMessage;
          break;
        case ERROR_EXIT:
          throw new PrivacyProviderInvalidInputException(input, getName());
        default:
          response = null;
      }
    } else {
      if (unspecifiedValueHandling == MaskingProviderConfig.UNSPECIFIED_VALUE_HANDLING_RANDOM) {
        response = randomGenerator == null ? null : randomGenerator.get();
      } else if (unspecifiedValueHandling == MaskingProviderConfig.UNSPECIFIED_VALUE_HANDLING_MESSAGE) {
        response = unspecifiedValueReturnMessage;
      } else {
        response = null;
      }
    }

    return response;
  }

  protected boolean isUnexpectedValueHandlingRandom() {
    // unexpectedInputHandler has priority
    return unexpectedInputHandler == null
        ? unspecifiedValueHandling == MaskingProviderConfig.UNSPECIFIED_VALUE_HANDLING_RANDOM
        : unexpectedInputHandler == UnexpectedMaskingInputHandler.RANDOM;
  }
}