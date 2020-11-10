/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

public class MaintainMaskingProvider extends AbstractMaskingProvider {

	/** */
	private static final long serialVersionUID = -1799896601470556794L;

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    return identifier;
  }
}
