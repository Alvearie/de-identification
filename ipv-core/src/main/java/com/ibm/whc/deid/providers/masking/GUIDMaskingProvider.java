/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.util.UUID;
import com.ibm.whc.deid.shared.pojo.config.masking.GUIDMaskingProviderConfig;

public class GUIDMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -2402304401477755175L;

  /** Instantiates a new Guid masking provider. */
  public GUIDMaskingProvider() {
    super(new GUIDMaskingProviderConfig());
  }

  /**
   * Instantiates a new Guid masking provider.
   *
   * @param config a GUIDMaskingProviderConfig instance
   */
  public GUIDMaskingProvider(GUIDMaskingProviderConfig config) {
    super(config);
  }

  @Override
  public String mask(String identifier) {
    return UUID.randomUUID().toString();
  }
}
