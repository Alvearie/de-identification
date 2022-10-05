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
 * Obtains metadata for the LATITUDE_LONGITUDE masking provider.
 */
public class LongitudeLatitudeMaskingProviderMetadataBuilder
    extends MaskingProviderMetadataBuilderBase {

  @Override
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider, Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "maskFixedRadiusRandomDirection", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "maskDonutMasking", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "maskRandomWithinCircle", bundle,
        OptionType.Boolean, Boolean.TRUE.toString()));
    options.add(buildOption(provider.getIdentifier(), "offsetMaximumRadius", bundle,
        OptionType.Integer, "100"));
    options.add(buildOption(provider.getIdentifier(), "offsetMinimumRadius", bundle,
        OptionType.Integer, "50"));
    return options;
  }
}
