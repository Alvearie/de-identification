/*
 * (C) Copyright Merative 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint.metadata;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import org.springframework.stereotype.Service;
import com.ibm.whc.deid.providers.masking.metadata.MaskingProviderMetadataFactory;
import com.ibm.whc.deid.providers.masking.metadata.model.MaskingProviderMetadataModel;
import com.ibm.whc.deid.providers.masking.metadata.model.ProvidersMetadata;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;

/**
 * Service for obtaining descriptive information about the supported privacy providers.
 */
@Service
public class MaskingProvidersMetadataService {

  /**
   * Returns descriptive information about the privacy providers, including a brief description,
   * configuration options and descriptions, and a link to the documentation.
   * 
   * @param locale The locale used to determine the language in which the information is to be
   *        returned. If information is not available in the indicated language, a default language
   *        will be used.
   */
  public ProvidersMetadata getMaskingProvidersMetadata(Locale locale) {
    MaskingProviderType[] providers = MaskingProviderType.values();
    List<MaskingProviderMetadataModel> metadataList = new ArrayList<>(providers.length);
    for (MaskingProviderType provider : providers) {
      MaskingProviderMetadataModel metadataModel =
          MaskingProviderMetadataFactory.getMaskingProviderMetadata(provider, locale);
      if (metadataModel != null) {
        metadataList.add(metadataModel);
      }
    }
    // sort the metadata alphabetically - UI request
    Collections.sort(metadataList,
        (first, second) -> first.getName().compareToIgnoreCase(second.getName()));
    ProvidersMetadata md = new ProvidersMetadata();
    md.setProviders(metadataList);
    return md;
  }
}
