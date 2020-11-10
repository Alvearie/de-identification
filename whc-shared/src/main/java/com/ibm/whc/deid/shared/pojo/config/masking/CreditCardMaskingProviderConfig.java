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
 * Masks a credit card number with the option to preserve the issuer (VISA, Mastercard, AMEX,
 * Discover, etc.)
 */
@JsonInclude(Include.NON_NULL)
public class CreditCardMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = -6363488510883018160L;
  boolean issuerPreserve = true;

  public CreditCardMaskingProviderConfig() {
    type = MaskingProviderType.CREDIT_CARD;
  }

  public boolean isIssuerPreserve() {
    return issuerPreserve;
  }

  public void setIssuerPreserve(boolean issuerPreserve) {
    this.issuerPreserve = issuerPreserve;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + (issuerPreserve ? 1231 : 1237);
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
    CreditCardMaskingProviderConfig other = (CreditCardMaskingProviderConfig) obj;
    if (issuerPreserve != other.issuerPreserve)
      return false;
    return true;
  }
}
