/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;

/*
 * Masks IP addresses with the option to preserve subnets.
 */
@JsonInclude(Include.NON_NULL)
public class IPAddressMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 7738870111730900534L;
  int subnetsPreserve = 0;

  public IPAddressMaskingProviderConfig() {
    type = MaskingProviderType.IP_ADDRESS;
  }

  public int getSubnetsPreserve() {
    return subnetsPreserve;
  }

  public void setSubnetsPreserve(int subnetsPreserve) {
    this.subnetsPreserve = subnetsPreserve;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + subnetsPreserve;
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
    IPAddressMaskingProviderConfig other = (IPAddressMaskingProviderConfig) obj;
    if (subnetsPreserve != other.subnetsPreserve)
      return false;
    return true;
  }
}
