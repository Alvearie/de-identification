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
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionGroupModel;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel.OptionType;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;

/**
 * Obtains metadata for the DATETIME masking provider.
 */
public class DateTimeMaskingProviderMetadataBuilder extends MaskingProviderMetadataBuilderBase {

  @Override
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider, Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    options
        .add(buildOption(provider.getIdentifier(), "formatFixed", bundle, OptionType.String, null));
    return options;
  }

  @Override
  protected List<ConfigurationOptionGroupModel> getOptionGroups(MaskingProviderTypes provider,
      Locale locale) {
    ResourceBundle bundle = getMetadataResourceBundle(locale);
    List<ConfigurationOptionGroupModel> groups = new ArrayList<>();
    ConfigurationOptionGroupModel group =
        buildOptionGroup(provider.getIdentifier(), "yearOverride", bundle);
    List<ConfigurationOptionModel> options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "overrideMask", bundle, OptionType.Boolean,
        Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "overrideYearsPassed", bundle,
        OptionType.Integer, "0"));
    options.add(
        buildOption(provider.getIdentifier(), "overrideValue", bundle, OptionType.String, null));
    group.setGroupOptions(options);
    groups.add(group);
    group = buildOptionGroup(provider.getIdentifier(), "shift", bundle);
    options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "maskShiftDate", bundle, OptionType.Boolean,
        Boolean.FALSE.toString()));
    options.add(
        buildOption(provider.getIdentifier(), "maskShiftSeconds", bundle, OptionType.Integer, "0"));
    group.setGroupOptions(options);
    groups.add(group);
    group = buildOptionGroup(provider.getIdentifier(), "generalization", bundle);
    options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "generalizeWeekyear", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "generalizeMonthyear", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "generalizeQuarteryear", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "generalizeYear", bundle, OptionType.Boolean,
        Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "generalizeNyearinterval", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "generalizeNyearintervalvalue", bundle,
        OptionType.Integer, "0"));
    options.add(buildOption(provider.getIdentifier(), "generalizeNyearintervalstart", bundle,
        OptionType.Integer, "0"));
    options.add(buildOption(provider.getIdentifier(), "generalizeNyearintervalend", bundle,
        OptionType.Integer, null));
    options.add(buildOption(provider.getIdentifier(), "generalizeYearMaskAgeOver90", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "generalizeMonthyearMaskAgeOver90", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    group.setGroupOptions(options);
    groups.add(group);
    group = buildOptionGroup(provider.getIdentifier(), "year", bundle);
    options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "yearMask", bundle, OptionType.Boolean,
        Boolean.TRUE.toString()));
    options.add(
        buildOption(provider.getIdentifier(), "yearRangeDown", bundle, OptionType.Integer, "10"));
    options
        .add(buildOption(provider.getIdentifier(), "yearRangeUp", bundle, OptionType.Integer, "0"));
    group.setGroupOptions(options);
    groups.add(group);
    group = buildOptionGroup(provider.getIdentifier(), "month", bundle);
    options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "monthMask", bundle, OptionType.Boolean,
        Boolean.TRUE.toString()));
    options.add(
        buildOption(provider.getIdentifier(), "monthRangeDown", bundle, OptionType.Integer, "12"));
    options.add(
        buildOption(provider.getIdentifier(), "monthRangeUp", bundle, OptionType.Integer, "0"));
    group.setGroupOptions(options);
    groups.add(group);
    group = buildOptionGroup(provider.getIdentifier(), "day", bundle);
    options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "dayMask", bundle, OptionType.Boolean,
        Boolean.TRUE.toString()));
    options.add(
        buildOption(provider.getIdentifier(), "dayRangeDownMin", bundle, OptionType.Integer, "0"));
    options.add(
        buildOption(provider.getIdentifier(), "dayRangeDown", bundle, OptionType.Integer, "7"));
    options.add(
        buildOption(provider.getIdentifier(), "dayRangeUpMin", bundle, OptionType.Integer, "0"));
    options
        .add(buildOption(provider.getIdentifier(), "dayRangeUp", bundle, OptionType.Integer, "0"));
    group.setGroupOptions(options);
    groups.add(group);
    group = buildOptionGroup(provider.getIdentifier(), "hour", bundle);
    options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "hourMask", bundle, OptionType.Boolean,
        Boolean.TRUE.toString()));
    options.add(
        buildOption(provider.getIdentifier(), "hourRangeDown", bundle, OptionType.Integer, "100"));
    options
        .add(buildOption(provider.getIdentifier(), "hourRangeUp", bundle, OptionType.Integer, "0"));
    group.setGroupOptions(options);
    groups.add(group);
    group = buildOptionGroup(provider.getIdentifier(), "minute", bundle);
    options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "minutesMask", bundle, OptionType.Boolean,
        Boolean.TRUE.toString()));
    options.add(buildOption(provider.getIdentifier(), "minutesRangeDown", bundle,
        OptionType.Integer, "100"));
    options.add(
        buildOption(provider.getIdentifier(), "minutesRangeUp", bundle, OptionType.Integer, "0"));
    group.setGroupOptions(options);
    groups.add(group);
    group = buildOptionGroup(provider.getIdentifier(), "second", bundle);
    options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "secondsMask", bundle, OptionType.Boolean,
        Boolean.TRUE.toString()));
    options.add(buildOption(provider.getIdentifier(), "secondsRangeDown", bundle,
        OptionType.Integer, "100"));
    options.add(
        buildOption(provider.getIdentifier(), "secondsRangeUp", bundle, OptionType.Integer, "0"));
    group.setGroupOptions(options);
    groups.add(group);
    group = buildOptionGroup(provider.getIdentifier(), "maxYearsAgoGroup", bundle);
    options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "yearMaxYearsAgoMask", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(
        buildOption(provider.getIdentifier(), "yearMaxYearsAgo", bundle, OptionType.Integer, "0"));
    options.add(buildOption(provider.getIdentifier(), "yearShiftFromCurrentYear", bundle,
        OptionType.Integer, "0"));
    options.add(buildOption(provider.getIdentifier(), "dayMaxDaysAgoMask", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(
        buildOption(provider.getIdentifier(), "dayMaxDaysAgo", bundle, OptionType.Integer, "0"));
    options.add(buildOption(provider.getIdentifier(), "dayShiftFromCurrentDay", bundle,
        OptionType.Integer, "0"));
    options.add(buildOption(provider.getIdentifier(), "yearMaxYearsAgoOnlyYear", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    group.setGroupOptions(options);
    groups.add(group);
    group = buildOptionGroup(provider.getIdentifier(), "deleteYear", bundle);
    options = new ArrayList<>();
    options.add(buildOption(provider.getIdentifier(), "yearDelete", bundle, OptionType.Boolean,
        Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "yearDeleteNdays", bundle, OptionType.Boolean,
        Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "yearDeleteNinterval", bundle,
        OptionType.Boolean, Boolean.FALSE.toString()));
    options.add(buildOption(provider.getIdentifier(), "yearDeleteNdaysValue", bundle,
        OptionType.Integer, "365"));
    options.add(buildOption(provider.getIdentifier(), "yearDeleteNointervalComparedateValue",
        bundle, OptionType.String, null));
    group.setGroupOptions(options);
    groups.add(group);
    return groups;
  }

}
