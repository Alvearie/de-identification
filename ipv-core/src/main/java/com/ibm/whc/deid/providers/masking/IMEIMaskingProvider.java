/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.providers.identifiers.IMEIIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.IMEIMaskingProviderConfig;
import com.ibm.whc.deid.util.IMEIManager;
import com.ibm.whc.deid.util.RandomGenerators;

/**
 * Privacy provider to mask International Mobile Equipment Identity (IMEI) device identifiers.
 */
public class IMEIMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -5189348513611132622L;

  private static final IMEIIdentifier imeiIdentifier = new IMEIIdentifier();
  private static final IMEIManager imeiManager = new IMEIManager();

  private final boolean preserveTAC;
  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;

  /**
   * Instantiates a new IMEI masking provider.
   *
   * @param config privacy provider configuration
   */
  public IMEIMaskingProvider(IMEIMaskingProviderConfig config) {
    this.preserveTAC = config.getPreserveTAC();
    unspecifiedValueHandling = config.getUnspecifiedValueHandling();
    unspecifiedValueReturnMessage = config.getUnspecifiedValueReturnMessage();
  }

  @Override
  public String mask(String identifier) {

    if (identifier == null) {
      debugFaultyInput("null");
      return null;
    }

    boolean preserve = this.preserveTAC;

    if (!IMEIMaskingProvider.imeiIdentifier.isOfThisType(identifier)) {
      debugFaultyInput("imei");
      if (unspecifiedValueHandling == 2) {
        preserve = false;
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    String tac;
    if (!preserve) {
      tac = imeiManager.getRandomKey();
    } else {
      tac = identifier.substring(0, 8);
    }

    String body = tac + RandomGenerators.generateRandomDigitSequence(6);
    body += (char) ('0' + RandomGenerators.luhnCheckDigit(body));

    return body;
  }
}
