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
import com.ibm.whc.deid.providers.masking.metadata.MaskingProviderMetadataBuilderBase;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel.OptionType;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;

/**
 * Obtains metadata for the URL masking provider.
 */
public class URLMaskingProviderMetadataBuilder extends MaskingProviderMetadataBuilderBase {

  @Override
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider,
      Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    options.add(
        buildOption(provider.getIdentifier(), "preserveDomains", bundle, OptionType.Integer, "1"));
    options.add(buildOption(provider.getIdentifier(), "maskPort", bundle, OptionType.Boolean,
        Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "maskRemoveQuery", bundle, OptionType.Boolean,
        Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "maskMaskQuery", bundle, OptionType.Boolean,
        Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "maskUsernamePassword", bundle,
        OptionType.Boolean, Boolean.TRUE.toString()));
    return options;
  }
}
