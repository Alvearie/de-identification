/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/*
 * Masks an original data value by replacing it with a randomly generated data pseudonym.
 */
@JsonInclude(Include.NON_NULL)
public class PseudonymMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 1084054409781390000L;
  private boolean generateViaOptionsEnabled = true;
  private int generateViaOptionsMinLength = 10;
  private int generateViaOptionsMaxLength = 10;
  private boolean generateViaOptionsGenerateUppercase = true;
  private boolean generateViaOptionsGenerateLowercase = true;
  private boolean generateViaOptionsGenerateDigit = true;
  private boolean generateViaOptionsGenerateSpecial = false;
  private boolean generateViaPatternEnabled = true;
  private String generateViaPatternPattern = null;
  private String generateViaPatternLanguageCode = "EN";
  private String generateViaPatternPatternName = null;
  private boolean generateViaHashEnabled = false;
  private boolean generateViaHashUseSHA256 = false;

  public PseudonymMaskingProviderConfig() {
    type = MaskingProviderType.PSEUDONYM;
  }

  public boolean isGenerateViaOptionsEnabled() {
    return generateViaOptionsEnabled;
  }

  public void setGenerateViaOptionsEnabled(boolean generateViaOptionsEnabled) {
    this.generateViaOptionsEnabled = generateViaOptionsEnabled;
  }

  public int getGenerateViaOptionsMinLength() {
    return generateViaOptionsMinLength;
  }

  public void setGenerateViaOptionsMinLength(int generateViaOptionsMinLength) {
    this.generateViaOptionsMinLength = generateViaOptionsMinLength;
  }

  public int getGenerateViaOptionsMaxLength() {
    return generateViaOptionsMaxLength;
  }

  public void setGenerateViaOptionsMaxLength(int generateViaOptionsMaxLength) {
    this.generateViaOptionsMaxLength = generateViaOptionsMaxLength;
  }

  public boolean isGenerateViaOptionsGenerateUppercase() {
    return generateViaOptionsGenerateUppercase;
  }

  public void setGenerateViaOptionsGenerateUppercase(boolean generateViaOptionsGenerateUppercase) {
    this.generateViaOptionsGenerateUppercase = generateViaOptionsGenerateUppercase;
  }

  public boolean isGenerateViaOptionsGenerateLowercase() {
    return generateViaOptionsGenerateLowercase;
  }

  public void setGenerateViaOptionsGenerateLowercase(boolean generateViaOptionsGenerateLowercase) {
    this.generateViaOptionsGenerateLowercase = generateViaOptionsGenerateLowercase;
  }

  public boolean isGenerateViaOptionsGenerateDigit() {
    return generateViaOptionsGenerateDigit;
  }

  public void setGenerateViaOptionsGenerateDigit(boolean generateViaOptionsGenerateDigit) {
    this.generateViaOptionsGenerateDigit = generateViaOptionsGenerateDigit;
  }

  public boolean isGenerateViaOptionsGenerateSpecial() {
    return generateViaOptionsGenerateSpecial;
  }

  public void setGenerateViaOptionsGenerateSpecial(boolean generateViaOptionsGenerateSpecial) {
    this.generateViaOptionsGenerateSpecial = generateViaOptionsGenerateSpecial;
  }

  public boolean isGenerateViaPatternEnabled() {
    return generateViaPatternEnabled;
  }

  public void setGenerateViaPatternEnabled(boolean generateViaPatternEnabled) {
    this.generateViaPatternEnabled = generateViaPatternEnabled;
  }

  public String getGenerateViaPatternPattern() {
    return generateViaPatternPattern;
  }

  public void setGenerateViaPatternPattern(String generateViaPatternPattern) {
    this.generateViaPatternPattern = generateViaPatternPattern;
  }

  public String getGenerateViaPatternLanguageCode() {
    return generateViaPatternLanguageCode;
  }

  public void setGenerateViaPatternLanguageCode(String generateViaPatternLanguageCode) {
    this.generateViaPatternLanguageCode = generateViaPatternLanguageCode;
  }

  public String getGenerateViaPatternPatternName() {
    return generateViaPatternPatternName;
  }

  public void setGenerateViaPatternPatternName(String generateViaPatternPatternName) {
    this.generateViaPatternPatternName = generateViaPatternPatternName;
  }

  public boolean isGenerateViaHashEnabled() {
    return generateViaHashEnabled;
  }

  public void setGenerateViaHashEnabled(boolean generateViaHashEnabled) {
    this.generateViaHashEnabled = generateViaHashEnabled;
  }

  public boolean isGenerateViaHashUseSHA256() {
    return generateViaHashUseSHA256;
  }

  public void setGenerateViaHashUseSHA256(boolean generateViaHashUseSHA256) {
    this.generateViaHashUseSHA256 = generateViaHashUseSHA256;
  }

  @Override
  public void validate() throws InvalidMaskingConfigurationException {
    super.validate();
    if (generateViaPatternLanguageCode == null) {
      throw new InvalidMaskingConfigurationException(
          "`generateViaPatternLanguageCode` must not be null");
    }
    if (generateViaOptionsMinLength < 1) {
      throw new InvalidMaskingConfigurationException(
          "`generateViaOptionsMinLength` must be greater than or equal to 1");
    }
    if (generateViaOptionsMaxLength < 1) {
      throw new InvalidMaskingConfigurationException(
          "`generateViaOptionsMaxLength` must be greater than or equal to 1");
    }
    if (generateViaOptionsMinLength > generateViaOptionsMaxLength) {
      throw new InvalidMaskingConfigurationException(
          "`generateViaOptionsMaxLength` must be greater than or equal to `generateViaOptionsMinLength`");
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (generateViaHashEnabled ? 1231 : 1237);
    result = prime * result + (generateViaHashUseSHA256 ? 1231 : 1237);
    result = prime * result + (generateViaOptionsEnabled ? 1231 : 1237);
    result = prime * result + (generateViaOptionsGenerateDigit ? 1231 : 1237);
    result = prime * result + (generateViaOptionsGenerateLowercase ? 1231 : 1237);
    result = prime * result + (generateViaOptionsGenerateSpecial ? 1231 : 1237);
    result = prime * result + (generateViaOptionsGenerateUppercase ? 1231 : 1237);
    result = prime * result + generateViaOptionsMaxLength;
    result = prime * result + generateViaOptionsMinLength;
    result = prime * result + (generateViaPatternEnabled ? 1231 : 1237);
    result = prime * result + ((generateViaPatternLanguageCode == null) ? 0
        : generateViaPatternLanguageCode.hashCode());
    result = prime * result
        + ((generateViaPatternPattern == null) ? 0 : generateViaPatternPattern.hashCode());
    result = prime * result
        + ((generateViaPatternPatternName == null) ? 0 : generateViaPatternPatternName.hashCode());
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
    PseudonymMaskingProviderConfig other = (PseudonymMaskingProviderConfig) obj;
    if (generateViaHashEnabled != other.generateViaHashEnabled)
      return false;
    if (generateViaHashUseSHA256 != other.generateViaHashUseSHA256)
      return false;
    if (generateViaOptionsEnabled != other.generateViaOptionsEnabled)
      return false;
    if (generateViaOptionsGenerateDigit != other.generateViaOptionsGenerateDigit)
      return false;
    if (generateViaOptionsGenerateLowercase != other.generateViaOptionsGenerateLowercase)
      return false;
    if (generateViaOptionsGenerateSpecial != other.generateViaOptionsGenerateSpecial)
      return false;
    if (generateViaOptionsGenerateUppercase != other.generateViaOptionsGenerateUppercase)
      return false;
    if (generateViaOptionsMaxLength != other.generateViaOptionsMaxLength)
      return false;
    if (generateViaOptionsMinLength != other.generateViaOptionsMinLength)
      return false;
    if (generateViaPatternEnabled != other.generateViaPatternEnabled)
      return false;
    if (generateViaPatternLanguageCode == null) {
      if (other.generateViaPatternLanguageCode != null)
        return false;
    } else if (!generateViaPatternLanguageCode.equals(other.generateViaPatternLanguageCode))
      return false;
    if (generateViaPatternPattern == null) {
      if (other.generateViaPatternPattern != null)
        return false;
    } else if (!generateViaPatternPattern.equals(other.generateViaPatternPattern))
      return false;
    if (generateViaPatternPatternName == null) {
      if (other.generateViaPatternPatternName != null)
        return false;
    } else if (!generateViaPatternPatternName.equals(other.generateViaPatternPatternName))
      return false;
    return true;
  }
}
