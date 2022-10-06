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
 * Obtains metadata for the ADDRESS masking provider.
 */
public class AddressMaskingProviderMetadataBuilder extends MaskingProviderMetadataBuilderBase {

  @Override
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider, Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "numberMask", bundle, OptionType.Boolean,
        Boolean.TRUE.toString()));
    options.add(buildOption(provider.getIdentifier(), "streetNameMask", bundle, OptionType.Boolean,
        Boolean.TRUE.toString()));
    options.add(buildOption(provider.getIdentifier(), "roadTypeMask", bundle, OptionType.Boolean,
        Boolean.TRUE.toString()));
    options.add(buildOption(provider.getIdentifier(), "cityMask", bundle, OptionType.Boolean,
        Boolean.TRUE.toString()));
    options.add(buildOption(provider.getIdentifier(), "countryMask", bundle, OptionType.Boolean,
        Boolean.TRUE.toString()));
    options.add(buildOption(provider.getIdentifier(), "postalCodeMask", bundle, OptionType.Boolean,
        Boolean.TRUE.toString()));
    options.add(buildOption(provider.getIdentifier(), "postalCodeNearest", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "postalCodeNearestK", bundle,
        OptionType.Integer, "10"));
    options.add(buildOption(provider.getIdentifier(), "maskPseudorandom", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    return options;
  }
}
