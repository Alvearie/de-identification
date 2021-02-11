/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/*
 * Replaces a phone or fax number with a random one.
 */
@JsonInclude(Include.NON_NULL)
public class PhoneMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -7578601586274424130L;

  private boolean countryCodePreserve = true;
  private boolean areaCodePreserve = true;
  private String invNdigitsReplaceWith = "1";

  List<String> phoneRegexPatterns;

  public PhoneMaskingProviderConfig() {
    type = MaskingProviderType.PHONE;
  }

  public boolean isCountryCodePreserve() {
    return countryCodePreserve;
  }

  public void setCountryCodePreserve(boolean countryCodePreserve) {
    this.countryCodePreserve = countryCodePreserve;
  }

  public boolean isAreaCodePreserve() {
    return areaCodePreserve;
  }

  public void setAreaCodePreserve(boolean areaCodePreserve) {
    this.areaCodePreserve = areaCodePreserve;
  }

  public String getInvNdigitsReplaceWith() {
    return invNdigitsReplaceWith;
  }

  public void setInvNdigitsReplaceWith(String invNdigitsReplaceWith) {
    this.invNdigitsReplaceWith = invNdigitsReplaceWith;
  }

  public List<String> getPhoneRegexPatterns() {
    return phoneRegexPatterns;
  }

  public void setPhoneRegexPatterns(List<String> phoneRegexPatterns) {
    this.phoneRegexPatterns = phoneRegexPatterns;
  }

  @Override
  public void validate() throws InvalidMaskingConfigurationException {
    super.validate();
    if (invNdigitsReplaceWith == null) {
      throw new InvalidMaskingConfigurationException("`invNdigitsReplaceWith` must be not null");
    }
    if (phoneRegexPatterns != null) {
      int offset = 0;
      for (String regx : phoneRegexPatterns) {
        if (regx == null || regx.trim().isEmpty()) {
          throw new InvalidMaskingConfigurationException(
              "pattern at offset " + offset + " in `phoneRegexPatterns` is empty");
        }
        try {
          Pattern.compile(regx);
        } catch (PatternSyntaxException e) {
          throw new InvalidMaskingConfigurationException("pattern at offset " + offset
              + " in `phoneRegexPatterns` is not valid: " + e.getMessage());
        }
        offset++;
      }
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (areaCodePreserve ? 1231 : 1237);
    result = prime * result + (countryCodePreserve ? 1231 : 1237);
    result =
        prime * result + ((invNdigitsReplaceWith == null) ? 0 : invNdigitsReplaceWith.hashCode());
    result = prime * result + ((phoneRegexPatterns == null) ? 0 : phoneRegexPatterns.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (!super.equals(obj))
      return false;
    if (getClass() != obj.getClass())
      return false;
    PhoneMaskingProviderConfig other = (PhoneMaskingProviderConfig) obj;
    if (areaCodePreserve != other.areaCodePreserve)
      return false;
    if (countryCodePreserve != other.countryCodePreserve)
      return false;
    if (invNdigitsReplaceWith == null) {
      if (other.invNdigitsReplaceWith != null)
        return false;
    } else if (!invNdigitsReplaceWith.equals(other.invNdigitsReplaceWith))
      return false;
    if (phoneRegexPatterns == null) {
      if (other.phoneRegexPatterns != null)
        return false;
    } else if (!phoneRegexPatterns.equals(other.phoneRegexPatterns))
      return false;
    return true;
  }
}
