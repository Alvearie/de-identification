/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.County;
import com.ibm.whc.deid.shared.pojo.config.masking.CountyMaskingProviderConfig;
import com.ibm.whc.deid.util.CountyManager;

public class CountyMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = 7519844993295356265L;

  protected CountyManager countyManager;
  protected final boolean getPseudorandom;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  protected volatile boolean initialized = false;

  /**
   * Instantiates a new Country masking provider.
   *
   * @param configuration the configuration
   * @param tenantId
   * @paramlocalizationProperty location of the localization property file
   * @param random the random
   */
  public CountyMaskingProvider(CountyMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty);
    this.getPseudorandom = configuration.isMaskPseudorandom();
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  protected void initialize() {
    if (!initialized) {
      countyManager = new CountyManager(null, localizationProperty);
      initialized = true;
    }
  }

  @Override
  public String mask(String identifier) {
    initialize();
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    if (this.getPseudorandom) {
      return countyManager.getPseudorandom(identifier);
    }

    County county = countyManager.getKey(identifier);

    if (county == null) {
      debugFaultyInput("county");
      if (unspecifiedValueHandling == 2) {
        return countyManager.getRandomKey();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    return countyManager.getRandomKey(county.getNameCountryCode());
  }
}
