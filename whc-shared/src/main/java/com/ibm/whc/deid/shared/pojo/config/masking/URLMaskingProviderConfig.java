/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/*
 * Masks URLs with the options to remove the query part, preserve domain levels, mask ports and
 * username / passwords that may be present.
 */
@JsonInclude(Include.NON_NULL)
public class URLMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -8611060486833375291L;
  private boolean maskPort = false;
  private boolean maskRemoveQuery = false;
  private int preserveDomains = 1;
  private boolean maskUsernamePassword = true;
  private boolean maskMaskQuery = false;

  public URLMaskingProviderConfig() {
    type = MaskingProviderType.URL;
  }

  public boolean isMaskPort() {
    return maskPort;
  }

  public void setMaskPort(boolean maskPort) {
    this.maskPort = maskPort;
  }

  public boolean isMaskRemoveQuery() {
    return maskRemoveQuery;
  }

  public void setMaskRemoveQuery(boolean maskRemoveQuery) {
    this.maskRemoveQuery = maskRemoveQuery;
  }

  public int getPreserveDomains() {
    return preserveDomains;
  }

  public void setPreserveDomains(int preserveDomains) {
    this.preserveDomains = preserveDomains;
  }

  public boolean isMaskUsernamePassword() {
    return maskUsernamePassword;
  }

  public void setMaskUsernamePassword(boolean maskUsernamePassword) {
    this.maskUsernamePassword = maskUsernamePassword;
  }

  public boolean isMaskMaskQuery() {
    return maskMaskQuery;
  }

  public void setMaskMaskQuery(boolean maskMaskQuery) {
    this.maskMaskQuery = maskMaskQuery;
  }

  @Override
  public void validate(DeidMaskingConfig maskingConfig)
      throws InvalidMaskingConfigurationException {
    super.validate(maskingConfig);
    if (preserveDomains < -1) {
      throw new InvalidMaskingConfigurationException("`preserveDomains` must not be less than -1");
    }
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (maskMaskQuery ? 1231 : 1237);
    result = prime * result + (maskPort ? 1231 : 1237);
    result = prime * result + (maskRemoveQuery ? 1231 : 1237);
    result = prime * result + (maskUsernamePassword ? 1231 : 1237);
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
    URLMaskingProviderConfig other = (URLMaskingProviderConfig) obj;
    if (maskMaskQuery != other.maskMaskQuery)
      return false;
    if (maskPort != other.maskPort)
      return false;
    if (maskRemoveQuery != other.maskRemoveQuery)
      return false;
    if (maskUsernamePassword != other.maskUsernamePassword)
      return false;
    if (preserveDomains != other.preserveDomains)
      return false;
    return true;
  }
}
