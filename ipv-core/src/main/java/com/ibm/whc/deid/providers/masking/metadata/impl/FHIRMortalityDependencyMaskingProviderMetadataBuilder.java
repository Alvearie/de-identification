/*
 * (C) Copyright Merative 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.metadata.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;
import com.ibm.whc.deid.providers.masking.metadata.MaskingProviderMetadataBuilderNoCommon;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel.OptionType;
import com.ibm.whc.deid.providers.masking.metadata.model.MaskingProviderMetadataModel;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;

/**
 * Obtains metadata for the FHIRMortalityDependencyMaskingProvider masking provider.
 */
public class FHIRMortalityDependencyMaskingProviderMetadataBuilder
    extends MaskingProviderMetadataBuilderNoCommon {

  @Override
  public MaskingProviderMetadataModel getMaskingProviderMetadata(MaskingProviderTypes provider,
      Locale locale) {
    MaskingProviderMetadataModel model = super.getMaskingProviderMetadata(provider, locale);
    // Modifies additional fields in the input document, not supported for UI individual rule
    // preview
    model.setRulePreview(false);
    return model;
  }

  @Override
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider, Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "mortalityIndicatorMinYears", bundle,
        OptionType.Integer, "2"));
    return options;
  }
}
