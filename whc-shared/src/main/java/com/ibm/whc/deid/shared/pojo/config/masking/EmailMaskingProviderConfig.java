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
 * Masks e-mail addresses with the option to preserve certain levels of the host domain.
 */
@JsonInclude(Include.NON_NULL)
public class EmailMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -6728740718420993380L;

  private int preserveDomains = 1;
  private int nameLength = -1;

  public EmailMaskingProviderConfig() {
    type = MaskingProviderType.EMAIL;
  }

  public int getPreserveDomains() {
    return preserveDomains;
  }

  public void setPreserveDomains(int preserveDomains) {
    this.preserveDomains = preserveDomains;
  }

  public int getNameLength() {
    return nameLength;
  }

  public void setNameLength(int nameLength) {
    this.nameLength = nameLength;
  }

  @Override
  public void validate() throws InvalidMaskingConfigurationException {
    if (nameLength < -1 || nameLength == 0) {
      throw new InvalidMaskingConfigurationException("`nameLength` must be -1 or greater than 0");
    }
    if (preserveDomains < -1) {
      throw new InvalidMaskingConfigurationException("`preserveDomains` must not be less than -1");
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + nameLength;
    result = prime * result + preserveDomains;
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
    EmailMaskingProviderConfig other = (EmailMaskingProviderConfig) obj;
    if (nameLength != other.nameLength)
      return false;
    if (preserveDomains != other.preserveDomains)
      return false;
    return true;
  }
}
