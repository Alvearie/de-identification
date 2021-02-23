/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.Race;
import com.ibm.whc.deid.shared.pojo.config.masking.RaceEthnicityMaskingProviderConfig;
import com.ibm.whc.deid.util.RaceManager;

/**
 * The type Race ethnicity masking provider.
 *
 */
public class RaceEthnicityMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -101305368159790864L;

  protected RaceManager raceManager;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  protected volatile boolean initialized = false;

  /**
   * @param configuration
 * @param tenantId
 * @param localizationProperty TODO
   */
  public RaceEthnicityMaskingProvider(RaceEthnicityMaskingProviderConfig configuration,
      String tenantId, String localizationProperty) {
		super(tenantId, localizationProperty);
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  @Override
  public String mask(String identifier) {
    initialize();
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    Race race = raceManager.getKey(identifier);

    if (race == null) {
      debugFaultyInput("race");
      if (unspecifiedValueHandling == 2) {
        return raceManager.getRandomKey();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    return raceManager.getRandomKey(race.getNameCountryCode());
  }

  protected void initialize() {
    if (!initialized) {
      raceManager = new RaceManager(null, localizationProperty);
      initialized = true;
    }
  }

}
