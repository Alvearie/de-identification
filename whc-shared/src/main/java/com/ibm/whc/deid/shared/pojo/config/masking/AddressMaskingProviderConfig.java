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
 * Masks an input postal address with a random one. Various elements of the address can be
 * preserved, like street names and road types.
 */
@JsonInclude(Include.NON_NULL)
public class AddressMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -1835358022810146037L;

  private boolean postalCodeNearest;
  private boolean roadTypeMask = true;
  private int postalCodeNearestK = 10;
  private boolean countryMask = true;
  private boolean postalCodeMask = true;
  private boolean numberMask = true;
  private boolean cityMask = true;
  private boolean maskPseudorandom;
  private boolean streetNameMask = true;
  private boolean countryMaskPseudorandom;
  private boolean cityMaskPseudorandom;

  CityMaskingProviderConfig cityMaskingConfig = new CityMaskingProviderConfig();

  CountryMaskingProviderConfig countryMaskingConfig = new CountryMaskingProviderConfig();;

  public AddressMaskingProviderConfig() {
    type = MaskingProviderType.ADDRESS;
  }

  public boolean isPostalCodeNearest() {
    return postalCodeNearest;
  }

  public void setPostalCodeNearest(boolean postalCodeNearest) {
    this.postalCodeNearest = postalCodeNearest;
  }

  public boolean isRoadTypeMask() {
    return roadTypeMask;
  }

  public void setRoadTypeMask(boolean roadTypeMask) {
    this.roadTypeMask = roadTypeMask;
  }

  public int getPostalCodeNearestK() {
    return postalCodeNearestK;
  }

  public void setPostalCodeNearestK(int postalCodeNearestK) {
    this.postalCodeNearestK = postalCodeNearestK;
  }

  public boolean isCountryMask() {
    return countryMask;
  }

  public void setCountryMask(boolean countryMask) {
    this.countryMask = countryMask;
  }

  public boolean isPostalCodeMask() {
    return postalCodeMask;
  }

  public void setPostalCodeMask(boolean postalCodeMask) {
    this.postalCodeMask = postalCodeMask;
  }

  public boolean isNumberMask() {
    return numberMask;
  }

  public void setNumberMask(boolean numberMask) {
    this.numberMask = numberMask;
  }

  public boolean isCityMask() {
    return cityMask;
  }

  public void setCityMask(boolean cityMask) {
    this.cityMask = cityMask;
  }

  public boolean isMaskPseudorandom() {
    return maskPseudorandom;
  }

  public void setMaskPseudorandom(boolean maskPseudorandom) {
    this.maskPseudorandom = maskPseudorandom;
    cityMaskingConfig.setMaskPseudorandom(maskPseudorandom);
    countryMaskingConfig.setMaskPseudorandom(maskPseudorandom);
  }

  public boolean isStreetNameMask() {
    return streetNameMask;
  }

  public void setStreetNameMask(boolean streetNameMask) {
    this.streetNameMask = streetNameMask;
  }

  public boolean isCountryMaskPseudorandom() {
    return countryMaskPseudorandom;
  }

  public void setCountryMaskPseudorandom(boolean countryMaskPseudorandom) {
    this.countryMaskPseudorandom = countryMaskPseudorandom;
  }

  public boolean isCityMaskPseudorandom() {
    return cityMaskPseudorandom;
  }

  public void setCityMaskPseudorandom(boolean cityMaskPseudorandom) {
    this.cityMaskPseudorandom = cityMaskPseudorandom;
  }

  public CityMaskingProviderConfig getCityMaskingConfig() {
    return cityMaskingConfig;
  }

  public void setCityMaskingConfig(CityMaskingProviderConfig cityMaskingConfig) {
    this.cityMaskingConfig = cityMaskingConfig;
  }

  public CountryMaskingProviderConfig getCountryMaskingConfig() {
    return countryMaskingConfig;
  }

  public void setCountryMaskingConfig(CountryMaskingProviderConfig countryMaskingConfig) {
    this.countryMaskingConfig = countryMaskingConfig;
  }

  @Override
  public void validate() throws InvalidMaskingConfigurationException {
    if (getPostalCodeNearestK() < 1) {
      throw new InvalidMaskingConfigurationException("`postalCodeNearestK` must be greater than 0");
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (cityMask ? 1231 : 1237);
    result = prime * result + (cityMaskPseudorandom ? 1231 : 1237);
    result = prime * result + ((cityMaskingConfig == null) ? 0 : cityMaskingConfig.hashCode());
    result = prime * result + (countryMask ? 1231 : 1237);
    result = prime * result + (countryMaskPseudorandom ? 1231 : 1237);
    result =
        prime * result + ((countryMaskingConfig == null) ? 0 : countryMaskingConfig.hashCode());
    result = prime * result + (maskPseudorandom ? 1231 : 1237);
    result = prime * result + (numberMask ? 1231 : 1237);
    result = prime * result + (postalCodeMask ? 1231 : 1237);
    result = prime * result + (postalCodeNearest ? 1231 : 1237);
    result = prime * result + postalCodeNearestK;
    result = prime * result + (roadTypeMask ? 1231 : 1237);
    result = prime * result + (streetNameMask ? 1231 : 1237);
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
    AddressMaskingProviderConfig other = (AddressMaskingProviderConfig) obj;
    if (cityMask != other.cityMask)
      return false;
    if (cityMaskPseudorandom != other.cityMaskPseudorandom)
      return false;
    if (cityMaskingConfig == null) {
      if (other.cityMaskingConfig != null)
        return false;
    } else if (!cityMaskingConfig.equals(other.cityMaskingConfig))
      return false;
    if (countryMask != other.countryMask)
      return false;
    if (countryMaskPseudorandom != other.countryMaskPseudorandom)
      return false;
    if (countryMaskingConfig == null) {
      if (other.countryMaskingConfig != null)
        return false;
    } else if (!countryMaskingConfig.equals(other.countryMaskingConfig))
      return false;
    if (maskPseudorandom != other.maskPseudorandom)
      return false;
    if (numberMask != other.numberMask)
      return false;
    if (postalCodeMask != other.postalCodeMask)
      return false;
    if (postalCodeNearest != other.postalCodeNearest)
      return false;
    if (postalCodeNearestK != other.postalCodeNearestK)
      return false;
    if (roadTypeMask != other.roadTypeMask)
      return false;
    if (streetNameMask != other.streetNameMask)
      return false;
    return true;
  }
}
