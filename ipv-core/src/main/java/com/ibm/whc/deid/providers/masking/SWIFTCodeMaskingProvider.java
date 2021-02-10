/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.SWIFTCode;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.SWIFTMaskingProviderConfig;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.SWIFTCodeManager;

public class SWIFTCodeMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -6934133173548691359L;

  protected volatile boolean initialized = false;
  protected volatile SWIFTCodeManager swiftCodeManager = null;

  protected final boolean preserveCountry;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  /**
   * Instantiates a new SWIFT masking provider.
   *
   * @param random the random
   * @param configuration the configuration
   */
  public SWIFTCodeMaskingProvider(SWIFTMaskingProviderConfig configuration, String tenantId) {
    this.preserveCountry = configuration.isPreserveCountry();
    unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  @Override
  public String mask(String identifier) {
    initialize();

    if (identifier == null) {
      debugFaultyInput("null");
      return null;
    }

    SWIFTCode swiftCode = swiftCodeManager.getKey(identifier);
    if (swiftCode == null) {
      debugFaultyInput("SWIFTcode");
      if (unspecifiedValueHandling == 2) {
        return swiftCodeManager.getRandomKey();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    if (this.preserveCountry) {
      return swiftCodeManager.getCodeFromCountry(identifier);
    }

    return swiftCodeManager.getRandomKey();
  }

  protected void initialize() {
    if (!initialized) {
      swiftCodeManager =
          (SWIFTCodeManager) ManagerFactory.getInstance().getManager(null, Resource.SWIFT, null);

      initialized = true;
    }
  }
}
