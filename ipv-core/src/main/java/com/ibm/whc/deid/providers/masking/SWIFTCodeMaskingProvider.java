/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.SWIFTMaskingProviderConfig;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.SWIFTCodeManager;

public class SWIFTCodeMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -6934133173548691359L;

  protected volatile boolean initialized = false;

	protected SWIFTCodeManager swiftCodeManager;
  protected final boolean preserveCountry;

  /**
   * Instantiates a new SWIFT masking provider.
 * @param configuration the configuration
 * @param localizationProperty TODO
 * @param random the random
   */
  public SWIFTCodeMaskingProvider(SWIFTMaskingProviderConfig configuration, String tenantId, String localizationProperty) {
    this.preserveCountry = configuration.isPreserveCountry();
		swiftCodeManager = (SWIFTCodeManager) ManagerFactory.getInstance().getManager(tenantId, Resource.SWIFT, null,
				localizationProperty);
  }

  @Override
  public String mask(String identifier) {
    initialize();
    if (this.preserveCountry) {
      return swiftCodeManager.getCodeFromCountry(identifier);
    }

    return swiftCodeManager.getRandomKey();
  }

  protected void initialize() {
    if (!initialized) {
      swiftCodeManager =
          (SWIFTCodeManager) ManagerFactory.getInstance().getManager(null, Resource.SWIFT, null, localizationProperty);

      initialized = true;
    }
  }

}
