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
 * Obtains metadata for the NUMBERVARIANCE masking provider.
 */
public class NumberVarianceMaskingProviderMetadataBuilder
    extends MaskingProviderMetadataBuilderBase {

  @Override
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider, Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "augmentMask", bundle, OptionType.Boolean,
        Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "augmentLowerBound", bundle,
        OptionType.Double, "1.0"));
    options.add(buildOption(provider.getIdentifier(), "augmentUpperBound", bundle,
        OptionType.Double, "10.0"));
    options.add(buildOption(provider.getIdentifier(), "resultWithPrecision", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(
        buildOption(provider.getIdentifier(), "precisionDigits", bundle, OptionType.Integer, "-1"));
    options.add(
        buildOption(provider.getIdentifier(), "maskLimitUp", bundle, OptionType.Double, "10.0"));
    options.add(
        buildOption(provider.getIdentifier(), "maskLimitDown", bundle, OptionType.Double, "10.0"));
    return options;
  }
}
