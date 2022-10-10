/*
 * © Merative US L.P. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.metadata.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;
import com.ibm.whc.deid.providers.masking.metadata.MaskingProviderMetadataBuilderBase;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel.OptionType;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionPossibleValueModel;
import com.ibm.whc.deid.providers.masking.metadata.model.MaskRuleSetConfigurationOptionModel;
import com.ibm.whc.deid.providers.masking.metadata.model.MaskingProviderMetadataModel;
import com.ibm.whc.deid.shared.pojo.config.masking.conditional.ConditionOperator;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;

/**
 * Obtains metadata for the CONDITIONAL masking provider.
 */
public class ConditionalMaskingProviderMetadataBuilder extends MaskingProviderMetadataBuilderBase {

  public static class ConditionalRuleSetSubOptionModel extends ConfigurationOptionModel {

    private boolean display = true;

    public ConditionalRuleSetSubOptionModel() {
      super();
    }

    /**
     * @param nm name
     * @param displayNm display text
     * @param type data type
     * @param desc description
     * @param dflt default value
     */
    public ConditionalRuleSetSubOptionModel(String nm, String displayNm, OptionType type,
        String desc, String dflt) {
      super(nm, displayNm, type, desc, dflt);
    }

    /**
     * @return whether this option should be displayed to a user
     */
    public boolean isDisplay() {
      return display;
    }

    /**
     * @param display whether this option should be displayed to a user
     */
    public void setDisplay(boolean display) {
      this.display = display;
    }
  }


  @Override
  public MaskingProviderMetadataModel getMaskingProviderMetadata(MaskingProviderTypes provider,
      Locale locale) {
    MaskingProviderMetadataModel model = super.getMaskingProviderMetadata(provider, locale);
    model.setRulePreview(false);
    return model;
  }

  @Override
  protected boolean supportsCommonOptions(MaskingProviderTypes provider) {
    return false;
  }

  @Override
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider,
      Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    MaskRuleSetConfigurationOptionModel ruleSet = new MaskRuleSetConfigurationOptionModel();
    List<ConditionalRuleSetSubOptionModel> subOptions = new ArrayList<>();
    subOptions.add(new ConditionalRuleSetSubOptionModel("field",
        bundle.getString(buildDisplayProperty(provider.getIdentifier(), "maskRuleSet_field")),
        OptionType.String,
        bundle.getString(buildDescriptionProperty(provider.getIdentifier(), "maskRuleSet_field")),
        null));
    ConditionalRuleSetSubOptionModel subOption = new ConditionalRuleSetSubOptionModel("operator",
        bundle.getString(buildDisplayProperty(provider.getIdentifier(), "maskRuleSet_operator")),
        OptionType.String, bundle.getString(
            buildDescriptionProperty(provider.getIdentifier(), "maskRuleSet_operator")),
        null);
    List<ConfigurationOptionPossibleValueModel> possibleValues = new ArrayList<>();
    for (ConditionOperator oper : ConditionOperator.values()) {      
      possibleValues.add(buildOptionPossibleValue(provider.getIdentifier(), oper.getValue(), bundle));
    }
    subOption.setPossibleValues(possibleValues);
    subOptions.add(subOption);
    subOption = new ConditionalRuleSetSubOptionModel("type",
        bundle.getString(buildDisplayProperty(provider.getIdentifier(), "maskRuleSet_type")),
        OptionType.String,
        bundle.getString(buildDescriptionProperty(provider.getIdentifier(), "maskRuleSet_type")),
        "string");
    subOption.setDisplay(false);
    possibleValues = new ArrayList<>();
    possibleValues.add(buildOptionPossibleValue(provider.getIdentifier(), "string", bundle));
    subOption.setPossibleValues(possibleValues);
    subOptions.add(subOption);
    subOptions.add(new ConditionalRuleSetSubOptionModel("value",
        bundle.getString(buildDisplayProperty(provider.getIdentifier(), "maskRuleSet_value")),
        OptionType.String,
        bundle.getString(buildDescriptionProperty(provider.getIdentifier(), "maskRuleSet_value")),
        null));
    subOptions
        .add(new ConditionalRuleSetSubOptionModel("valueList",
            bundle
                .getString(buildDisplayProperty(provider.getIdentifier(), "maskRuleSet_valueList")),
            OptionType.StringArray,
            bundle.getString(
                buildDescriptionProperty(provider.getIdentifier(), "maskRuleSet_valueList")),
            null));
    subOption = new ConditionalRuleSetSubOptionModel("maskingProvider",
        bundle.getString(
            buildDisplayProperty(provider.getIdentifier(), "maskRuleSet_maskingProvider")),
        OptionType.MaskingProvider,
        bundle.getString(
            buildDescriptionProperty(provider.getIdentifier(), "maskRuleSet_maskingProvider")),
        null);
    subOption.setExcluded(Arrays.asList(MaskingProviderType.CONDITIONAL.getIdentifier()));
    subOptions.add(subOption);
    ruleSet.setSubOptions(subOptions);
    options.add(ruleSet);
    ConfigurationOptionModel option = buildOption(provider.getIdentifier(), "maskingProvider",
        bundle, OptionType.MaskingProvider, null);
    option.setExcluded(Arrays.asList(MaskingProviderType.CONDITIONAL.getIdentifier()));
    options.add(option);
    return options;
  }
}
