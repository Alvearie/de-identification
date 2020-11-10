/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.configuration.MaskingConfiguration;
import com.ibm.whc.deid.shared.pojo.config.masking.NullMaskingProviderConfig;
import java.security.SecureRandom;

/**
 * The type Null masking provider.
 *
 */
public class NullMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -663861245004131575L;

  boolean maskReturnNull;

  /** Instantiates a new Null masking provider. */
  public NullMaskingProvider() {
    this(new NullMaskingProviderConfig());
  }

  /**
   * Instantiates a new Null masking provider.
   *
   * @param random the random
   * @param maskingConfiguration the masking configuration
   */
  public NullMaskingProvider(SecureRandom random, MaskingConfiguration maskingConfiguration) {
    this(maskingConfiguration);
  }

  /**
   * Instantiates a new Null masking provider.
   *
   * @param configuration the configuration
   */
  public NullMaskingProvider(MaskingConfiguration configuration) {
    this.maskReturnNull = configuration.getBooleanValue("null.mask.returnNull");
  }

  public NullMaskingProvider(NullMaskingProviderConfig maskingConfiguration) {
    maskReturnNull = maskingConfiguration.isMaskReturnNull();
  }

  @Override
  public String mask(String identifier) {
    return maskReturnNull ? null : "";
  }
}
