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
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionPossibleValueModel;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel.OptionType;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.UsageType;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;

/**
 * Obtains metadata for the FPE masking provider.
 */
public class FPEMaskingProviderMetadataBuilder extends MaskingProviderMetadataBuilderBase {

  @Override
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider, Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "key", bundle, OptionType.String, null));
    options.add(buildOption(provider.getIdentifier(), "tweak", bundle, OptionType.String, null));
    ConfigurationOptionModel option =
        buildOption(provider.getIdentifier(), "inputType", bundle, OptionType.String, "DIGITS");
    List<ConfigurationOptionPossibleValueModel> possibleValues = new ArrayList<>();
    for (UsageType usage : UsageType.values()) {
      possibleValues.add(buildOptionPossibleValue(provider.getIdentifier(), usage.name(), bundle));
    }
    option.setPossibleValues(possibleValues);
    options.add(option);
    option = buildOption(provider.getIdentifier(), "padding", bundle, OptionType.String, "NONE");
    possibleValues = new ArrayList<>();
    for (Pad pad : Pad.values()) {
      possibleValues.add(buildOptionPossibleValue(provider.getIdentifier(), pad.name(), bundle));
    }
    option.setPossibleValues(possibleValues);
    options.add(option);
    return options;
  }
}
