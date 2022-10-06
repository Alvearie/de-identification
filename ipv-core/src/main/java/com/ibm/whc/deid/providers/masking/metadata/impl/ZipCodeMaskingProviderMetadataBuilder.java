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
 * Obtains metadata for the ZIPCODE masking provider.
 */
public class ZipCodeMaskingProviderMetadataBuilder extends MaskingProviderMetadataBuilderBase {

  @Override
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider,
      Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    options.add(
        buildOption(provider.getIdentifier(), "maskCountryCode", bundle, OptionType.String, "US"));
    options.add(buildOption(provider.getIdentifier(), "maskReplaceWithNeighbor", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "maskReplaceWithNeighborNearestCount", bundle,
        OptionType.Integer, "10"));
    options.add(
        buildOption(provider.getIdentifier(), "maskPrefixLength", bundle, OptionType.Integer, "3"));
    options.add(buildOption(provider.getIdentifier(), "maskPrefixRequireMinPopulation", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "maskTruncateIfNotMinPopulation", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "maskTruncateLengthIfNotMinPopulation",
        bundle, OptionType.Integer, "2"));
    options.add(buildOption(provider.getIdentifier(), "maskPrefixMinPopulation", bundle,
        OptionType.Integer, "20000"));
    options.add(buildOption(provider.getIdentifier(), "maskSuffixReplaceWithRandom", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "maskSuffixReplaceWithValidOnly", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "maskSuffixTruncate", bundle,
        OptionType.Boolean, Boolean.TRUE.toString()));    
    return options;
  }
}
