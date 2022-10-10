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
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionGroupModel;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel.OptionType;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;

/**
 * Obtains metadata for the PSEUDONYM masking provider.
 */
public class PseudonymMaskingProviderMetadataBuilder extends MaskingProviderMetadataBuilderBase {

  @Override
  protected boolean supportsCommonOptions(MaskingProviderTypes provider) {
    return false;
  }
  
  @Override
  protected List<ConfigurationOptionGroupModel> getOptionGroups(MaskingProviderTypes provider,
      Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionGroupModel> groups = new ArrayList<>();
    ConfigurationOptionGroupModel group =
        buildOptionGroup(provider.getIdentifier(), "random", bundle);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "generateViaOptionsEnabled", bundle,
        OptionType.Boolean, Boolean.TRUE.toString()));
    options.add(buildOption(provider.getIdentifier(), "generateViaOptionsMinLength", bundle,
        OptionType.Integer, "10"));
    options.add(buildOption(provider.getIdentifier(), "generateViaOptionsMaxLength", bundle,
        OptionType.Integer, "10"));
    options.add(buildOption(provider.getIdentifier(), "generateViaOptionsGenerateUppercase", bundle,
        OptionType.Boolean, Boolean.TRUE.toString()));
    options.add(buildOption(provider.getIdentifier(), "generateViaOptionsGenerateLowercase", bundle,
        OptionType.Boolean, Boolean.TRUE.toString()));
    options.add(buildOption(provider.getIdentifier(), "generateViaOptionsGenerateDigit", bundle,
        OptionType.Boolean, Boolean.TRUE.toString()));
    options.add(buildOption(provider.getIdentifier(), "generateViaOptionsGenerateSpecial", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    group.setGroupOptions(options);
    groups.add(group);
    group = buildOptionGroup(provider.getIdentifier(), "pattern", bundle);
    options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "generateViaPatternEnabled", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "generateViaPatternLanguageCode", bundle,
        OptionType.String, "en"));    
    options.add(buildOption(provider.getIdentifier(), "generateViaPatternPattern", bundle,        
        OptionType.String, null));
    options.add(buildOption(provider.getIdentifier(), "generateViaPatternPatternName", bundle,
        OptionType.String, null));    
    group.setGroupOptions(options);
    groups.add(group);
    group = buildOptionGroup(provider.getIdentifier(), "hash", bundle);
    options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "generateViaHashEnabled", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "generateViaHashUseSHA256", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    group.setGroupOptions(options);
    groups.add(group);
    return groups;
  }
}
