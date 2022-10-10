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
import com.ibm.whc.deid.providers.masking.metadata.model.MaskRuleSetConfigurationOptionModel;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;

/**
 * Obtains metadata for the GENERALIZE masking provider.
 */
public class GeneralizeMaskingProviderMetadataBuilder extends MaskingProviderMetadataBuilderBase {

  @Override
  protected boolean supportsCommonOptions(MaskingProviderTypes provider) {
    return false;
  }

  @Override
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider, Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    MaskRuleSetConfigurationOptionModel ruleSet = new MaskRuleSetConfigurationOptionModel();
    List<ConfigurationOptionModel> subOptions = new ArrayList<>();
    subOptions
        .add(buildOption(provider.getIdentifier(), "targetValue", bundle, OptionType.String, null));
    subOptions.add(buildOption(provider.getIdentifier(), "sourceValueIn", bundle,
        OptionType.StringArray, null));
    subOptions.add(buildOption(provider.getIdentifier(), "sourceValueNotIn", bundle,
        OptionType.StringArray, null));
    ruleSet.setSubOptions(subOptions);
    options.add(ruleSet);
    return options;
  }
}
