/*
 * © Merative US L.P. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.metadata;

import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;

/**
 * Base class for classes implementing the MaskingProviderMetadataBuilder interface that do not
 * support the common configuration options.
 */
public class MaskingProviderMetadataBuilderNoCommon extends MaskingProviderMetadataBuilderBase {

  @Override
  protected boolean supportsCommonOptions(MaskingProviderTypes provider) {
    return false;
  }
}
