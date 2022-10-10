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
import com.ibm.whc.deid.providers.masking.metadata.model.MaskingProviderMetadataModel;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;

/**
 * Obtains metadata for the DATETIME_CONSISTENT_SHIFT masking provider.
 */
public class DateTimeConsistentShiftMaskingProviderMetadataBuilder extends MaskingProviderMetadataBuilderBase {

  @Override
  public MaskingProviderMetadataModel getMaskingProviderMetadata(MaskingProviderTypes provider,
      Locale locale) {
    MaskingProviderMetadataModel model = super.getMaskingProviderMetadata(provider, locale);
    // requires access to additional field in the input document,
    // not supported for UI individual rule preview
    model.setRulePreview(false);
    return model;
  }

  @Override
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider, Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "patientIdentifierPath", bundle,
        OptionType.String, "/patient/reference"));
    options.add(buildOption(provider.getIdentifier(), "dateShiftMinimumDays", bundle,
        OptionType.Integer, "1"));
    options.add(buildOption(provider.getIdentifier(), "dateShiftMaximumDays", bundle,
        OptionType.Integer, "365"));
    ConfigurationOptionModel option = buildOption(provider.getIdentifier(), "dateShiftDirection",
        bundle, OptionType.String, "beforeOrAfter");
    List<ConfigurationOptionPossibleValueModel> possibleValues = new ArrayList<>();
    possibleValues.add(buildOptionPossibleValue(provider.getIdentifier(), "before", bundle));
    possibleValues.add(buildOptionPossibleValue(provider.getIdentifier(), "beforeOrAfter", bundle));
    possibleValues.add(buildOptionPossibleValue(provider.getIdentifier(), "after", bundle));
    option.setPossibleValues(possibleValues);
    options.add(option);
    options.add(buildOption(provider.getIdentifier(), "salt", bundle, OptionType.String, null));
    options.add(buildOption(provider.getIdentifier(), "customFormats", bundle,
        OptionType.StringArray, null));
    return options;
  }
}
