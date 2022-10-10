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
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionPossibleValueModel;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;

/**
 * Obtains metadata for the HASH masking provider.
 */
public class HashMaskingProviderMetadataBuilder extends MaskingProviderMetadataBuilderBase {

  @Override
  protected boolean supportsCommonOptions(MaskingProviderTypes provider) {
    return false;
  }

  @Override
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider, Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    ConfigurationOptionModel option = buildOption(provider.getIdentifier(), "algorithmDefault",
        bundle, OptionType.String, "SHA-256");
    List<ConfigurationOptionPossibleValueModel> possibleValues = new ArrayList<>();
    possibleValues.add(new ConfigurationOptionPossibleValueModel("MD2", "MD2"));
    possibleValues.add(new ConfigurationOptionPossibleValueModel("MD5", "MD5"));
    possibleValues.add(new ConfigurationOptionPossibleValueModel("SHA-1", "SHA-1"));
    possibleValues.add(new ConfigurationOptionPossibleValueModel("SHA-256", "SHA-256"));
    possibleValues.add(new ConfigurationOptionPossibleValueModel("SHA-384", "SHA-384"));
    possibleValues.add(new ConfigurationOptionPossibleValueModel("SHA-512", "SHA-512"));
    option.setPossibleValues(possibleValues);
    options.add(option);
    options.add(buildOption(provider.getIdentifier(), "salt", bundle, OptionType.String, null));
    options.add(buildOption(provider.getIdentifier(), "offsetOffsetMask", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "offsetOffsetMaskDelete", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(
        buildOption(provider.getIdentifier(), "offsetBegin", bundle, OptionType.Integer, "-1"));
    options
        .add(buildOption(provider.getIdentifier(), "offsetEnd", bundle, OptionType.Integer, "-1"));
    option = buildOption(provider.getIdentifier(), "offsetInvalidOffsetValue", bundle,
        OptionType.Integer, "1");
    possibleValues = new ArrayList<>();
    possibleValues.add(buildOptionPossibleValue(provider.getIdentifier(), "1", bundle));
    possibleValues.add(buildOptionPossibleValue(provider.getIdentifier(), "2", bundle));
    possibleValues.add(buildOptionPossibleValue(provider.getIdentifier(), "3", bundle));
    option.setPossibleValues(possibleValues);
    options.add(option);
    return options;
  }
}
