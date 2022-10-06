/*
 * (C) Copyright Merative 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.metadata;

import java.util.Locale;
import com.ibm.whc.deid.providers.masking.metadata.model.MaskingProviderMetadataModel;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;

/**
 * Interface supported by objects that can provide metadata about a privacy/masking provider.
 * 
 * @see MaskingProviderMetadataFactory
 */
public interface MaskingProviderMetadataBuilder {

  /**
   * Obtains information about a masking provider type.
   * 
   * @param provider the provider about which information is required
   * 
   * @param locale the preferred language in which the translatable or human-readable parts of the
   *        information are to be returned
   * 
   * @return the available information in structured form
   */
  public MaskingProviderMetadataModel getMaskingProviderMetadata(MaskingProviderTypes provider,
      Locale locale);

}
