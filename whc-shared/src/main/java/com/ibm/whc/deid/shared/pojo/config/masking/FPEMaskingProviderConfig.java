/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/**
 * Configuration for the Format-Preserving Encryption privacy provider.
 */
@JsonInclude(Include.NON_NULL)
public class FPEMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 1L;

  public enum UsageType {
    //@formatter:off
    DIGITS(true), 
    LETTERS_LOWER(true), 
    LETTERS_UPPER(true), 
    LETTERS_INSENSITIVE_AS_LOWER(true), 
    LETTERS_INSENSITIVE_AS_UPPER(true), 
    LETTERS_INSENSITIVE_AS_ORIGINAL(true), 
    LETTERS_SENSITIVE(false),
    DIGITS_LETTERS_LOWER(true),
    DIGITS_LETTERS_UPPER(true),
    DIGITS_LETTERS_INSENSITIVE_AS_LOWER(true),
    DIGITS_LETTERS_INSENSITIVE_AS_UPPER(true),
    DIGITS_LETTERS_SENSITIVE(false)
    ;
    //@formatter:on

    private final boolean padding;

    private UsageType(boolean p) {
      padding = p;
    }
    
    public boolean supportsPadding() {
      return padding;
    }
  }

  public enum Pad {

    /**
     * No padding - error if value has less than the minimum characters for the radix
     */
    NONE,

    /**
     * add pad characters as necessary to the beginning of the value
     */
    FRONT,

    /**
     * add pad characters as necessary to the end of the value
     */
    BACK
  }

  public static final UsageType DEFAULT_USAGE_TYPE = UsageType.DIGITS;
  public static final Pad DEFAULT_PADDING = Pad.NONE;
  public static final Pattern MATERIAL_CONTENT_PATTERN = Pattern.compile("[0-9a-f]+");

  private String key;
  private String tweak;
  private UsageType inputType;
  private Pad padding;

  public FPEMaskingProviderConfig() {
    inputType = DEFAULT_USAGE_TYPE;
    padding = DEFAULT_PADDING;
    type = MaskingProviderType.FPE;
  }

  public String getKey() {
    return key;
  }

  public void setKey(String key) {
    this.key = key;
  }

  public String getTweak() {
    return tweak;
  }

  public void setTweak(String tweak) {
    this.tweak = tweak;
  }

  public UsageType getInputType() {
    return inputType;
  }

  public void setInputType(UsageType inputType) {
    this.inputType = inputType == null ? DEFAULT_USAGE_TYPE : inputType;
  }

  public Pad getPadding() {
    return padding;
  }

  public void setPadding(Pad padding) {
    this.padding = padding == null ? DEFAULT_PADDING : padding;
  }

  // TODO: add equals() and hashcode()

  @Override
  public void validate(DeidMaskingConfig maskingConfig)
      throws InvalidMaskingConfigurationException {

    super.validate(maskingConfig);

    int length = key == null ? 0 : key.length();
    if (length != 32 && length != 48 && length != 64) {
      throw new InvalidMaskingConfigurationException("`key` must be 32, 48, or 64 characters");
    }
    Matcher matcher = MATERIAL_CONTENT_PATTERN.matcher(key);
    if (!matcher.matches()) {
      throw new InvalidMaskingConfigurationException(
          "`key` must contain only characters 0-9 and a-f");
    }

    length = tweak == null ? 0 : tweak.length();
    if (length != 16) {
      throw new InvalidMaskingConfigurationException("`tweak` must be 16 characters");
    }
    matcher = MATERIAL_CONTENT_PATTERN.matcher(tweak);
    if (!matcher.matches()) {
      throw new InvalidMaskingConfigurationException(
          "`tweak` must contain only characters 0-9 and a-f");
    }

    if (padding != Pad.NONE && !inputType.supportsPadding()) {
      if (!matcher.matches()) {
        throw new InvalidMaskingConfigurationException("`padding` has the value " + padding.name()
            + " but the `inputType` value " + inputType.name() + " does not support padding");
      }
    }
  }
}
