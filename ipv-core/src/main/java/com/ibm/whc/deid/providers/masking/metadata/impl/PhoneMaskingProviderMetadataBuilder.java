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
import com.ibm.whc.deid.providers.identifiers.PhoneIdentifier;
import com.ibm.whc.deid.providers.masking.metadata.MaskingProviderMetadataBuilderBase;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel.OptionType;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;

/**
 * Obtains metadata for the PHONE masking provider.
 */
public class PhoneMaskingProviderMetadataBuilder extends MaskingProviderMetadataBuilderBase {

  @Override
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider, Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "countryCodePreserve", bundle,
        OptionType.Boolean, Boolean.TRUE.toString()));
    options.add(buildOption(provider.getIdentifier(), "areaCodePreserve", bundle,
        OptionType.Boolean, Boolean.TRUE.toString()));
    boolean first = true;
    StringBuilder buffer = new StringBuilder(PhoneIdentifier.DEFAULT_PATTERN_STRINGS.size() * 100);
    for (String pattern : PhoneIdentifier.DEFAULT_PATTERN_STRINGS) {
      if (first) {
        first = false;
      } else {
        buffer.append(", ");  // separator used by User Interface
      }
      buffer.append(pattern);
    }
    options.add(buildOption(provider.getIdentifier(), "phoneRegexPatterns", bundle,
        OptionType.RegExArray, buffer.toString()));
    return options;
  }
}
