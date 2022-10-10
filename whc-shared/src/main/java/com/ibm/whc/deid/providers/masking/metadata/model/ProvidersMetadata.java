/*
 * © Merative US L.P. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.metadata.model;

import java.util.List;

/**
 * Container bean for returning descriptive information about the supported masking providers.
 */
public class ProvidersMetadata {

  private List<MaskingProviderMetadataModel> providers;

  /**
   * @return the providers
   */
  public List<MaskingProviderMetadataModel> getProviders() {
    return providers;
  }

  /**
   * @param providers the providers to set
   */
  public void setProviders(List<MaskingProviderMetadataModel> providers) {
    this.providers = providers;
  }
}
