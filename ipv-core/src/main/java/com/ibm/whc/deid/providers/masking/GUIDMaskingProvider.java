/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.pojo.config.masking.GUIDMaskingProviderConfig;
import java.util.UUID;

public class GUIDMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -2402304401477755175L;

  /** Instantiates a new Guid masking provider. */
  public GUIDMaskingProvider() {}

  /**
   * Instantiates a new Guid masking provider.
   *
   * @param config a GUIDMaskingProviderConfig instance
   */
  public GUIDMaskingProvider(GUIDMaskingProviderConfig config) {}

  @Override
  public String mask(String identifier) {
    return UUID.randomUUID().toString();
  }
}
