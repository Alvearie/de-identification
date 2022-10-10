/*
 * © Merative US L.P. 2022
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
 * Obtains metadata for the REDACT masking provider.
 */
public class RedactMaskingProviderMetadataBuilder extends MaskingProviderMetadataBuilderBase {

  @Override
  protected boolean supportsCommonOptions(MaskingProviderTypes provider) {
    return false;
  }

  @Override
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider, Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "preserveLength", bundle, OptionType.Boolean,
        Boolean.TRUE.toString()));
    options.add(
        buildOption(provider.getIdentifier(), "replaceCharacter", bundle, OptionType.String, "X"));
    return options;
  }
}
