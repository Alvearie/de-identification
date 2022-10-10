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
 * Obtains metadata for the REPLACE masking provider.
 */
public class ReplaceMaskingProviderMetadataBuilder extends MaskingProviderMetadataBuilderBase {

  @Override
  protected boolean supportsCommonOptions(MaskingProviderTypes provider) {
    return false;
  }
  
  @Override
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider,
      Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "maskReplaceWithRandom", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "maskReplaceWithAsterisks", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options
        .add(buildOption(provider.getIdentifier(), "maskOffset", bundle, OptionType.Integer, "0"));
    options.add(
        buildOption(provider.getIdentifier(), "maskPreserve", bundle, OptionType.Integer, "3"));
    return options;
  }
}
