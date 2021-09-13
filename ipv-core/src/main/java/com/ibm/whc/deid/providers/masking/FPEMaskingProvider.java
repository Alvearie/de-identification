/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.providers.masking.fpe.EncryptionEngineException;
import com.ibm.whc.deid.providers.masking.fpe.FPEDriver;
import com.ibm.whc.deid.providers.masking.fpe.UnsupportedLengthException;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.UsageType;

/**
 * Masks identifiers as per NIST Format-Preserving Encryption FF3-1.
 */
public class FPEMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 1L;

  private final String key;
  private final String tweak;
  private final UsageType usageType;
  private final Pad padding;

  /**
   * Instantiates a new FPE masking provider.
   *
   * @param config the configuration
   */
  public FPEMaskingProvider(FPEMaskingProviderConfig config) {
    super(config);
    this.key = config.getKey();
    this.tweak = config.getTweak();
    this.usageType = config.getInputType();
    this.padding = config.getPadding();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    try {
      return FPEDriver.getFPEDriver(usageType).encrypt(identifier, key, tweak, padding);

    } catch (UnsupportedLengthException e) {
      // TODO: add log message
      return applyUnexpectedValueHandling(identifier, null);
    } catch (EncryptionEngineException e) {
      // TODO: add log message
      throw new RuntimeException(e);
    }
  }
}
