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
import com.ibm.whc.deid.providers.masking.metadata.model.MaskingProviderMetadataModel;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel.OptionType;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;

/**
 * Obtains metadata for the DATEDEPENDENCY masking provider.
 */
public class DateDependencyMaskingProviderMetadataBuilder extends MaskingProviderMetadataBuilderBase {

  @Override
  public MaskingProviderMetadataModel getMaskingProviderMetadata(MaskingProviderTypes provider,
      Locale locale) {
    MaskingProviderMetadataModel model = super.getMaskingProviderMetadata(provider, locale);
    model.setRulePreview(false);
    return model;
  }

  @Override
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider, Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "datetimeYearDeleteNIntervalCompareDate", bundle, OptionType.String, null));
    options.add(buildOption(provider.getIdentifier(), "dateYearDeleteNDaysValue", bundle, OptionType.Integer, "365")); 
    return options;
  }
}
