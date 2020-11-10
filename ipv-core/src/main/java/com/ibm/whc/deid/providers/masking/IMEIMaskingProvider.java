/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.providers.identifiers.IMEIIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.IMEIMaskingProviderConfig;
import com.ibm.whc.deid.util.IMEIManager;
import com.ibm.whc.deid.util.RandomGenerators;

public class IMEIMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -5189348513611132622L;

  private static final IMEIIdentifier identifier = new IMEIIdentifier();
  private static final IMEIManager imeiManager = new IMEIManager();
  private final boolean preserveTAC;

  /**
   * Instantiates a new Imei masking provider.
   *
   * @param config an IMEIMaskingProviderConfig instance
   */
  public IMEIMaskingProvider(IMEIMaskingProviderConfig config) {
    this.preserveTAC = config.getPreserveTAC();
  }

  @Override
  public String mask(String identifier) {
    String tac;
    if (!IMEIMaskingProvider.identifier.isOfThisType(identifier)) {
      return null;
    } else {
      if (!this.preserveTAC) {
        tac = imeiManager.getRandomKey();
      } else {
        tac = identifier.substring(0, 8);
      }
    }

    String body = tac + RandomGenerators.generateRandomDigitSequence(6);
    body += (char) ('0' + RandomGenerators.luhnCheckDigit(body));

    return body;
  }
}
