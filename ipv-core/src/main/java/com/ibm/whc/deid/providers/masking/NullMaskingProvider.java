/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.pojo.config.masking.NullMaskingProviderConfig;

/**
 * The type Null masking provider.
 *
 */
public class NullMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -663861245004131575L;

  private final boolean maskReturnNull;

  /** Instantiates a new Null masking provider. */
  public NullMaskingProvider() {
    this(new NullMaskingProviderConfig());
  }

  /**
   * Instantiates a new Null masking provider.
   *
   * @param configuration the configuration
   */
  public NullMaskingProvider(NullMaskingProviderConfig maskingConfiguration) {
    maskReturnNull = maskingConfiguration.isMaskReturnNull();
  }

  @Override
  public String mask(String identifier) {
    return maskReturnNull ? null : "";
  }
}
