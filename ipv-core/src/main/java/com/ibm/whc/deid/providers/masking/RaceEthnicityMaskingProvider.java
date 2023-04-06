/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.Race;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.RaceEthnicityMaskingProviderConfig;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.RaceManager;

/**
 * The type race-ethnicity masking provider.
 *
 */
public class RaceEthnicityMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -101305368159790864L;

  protected transient volatile RaceManager raceResourceManager = null;

  /**
   * @param configuration the configuration of this privacy provider
   * @param tenantId the identifier of the tenant associated with this request
   * @param localizationProperty location of the localization property file
   */
  public RaceEthnicityMaskingProvider(RaceEthnicityMaskingProviderConfig configuration,
      String tenantId, String localizationProperty) {
    super(tenantId, localizationProperty, configuration);
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    Race randomRace = getRaceManager().getRandomValue();
    return randomRace == null ? null : randomRace.getName();
  }

  protected RaceManager getRaceManager() {
    if (raceResourceManager == null) {
      raceResourceManager = (RaceManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.RACE_ETHNICITY, null, localizationProperty);
    }
    return raceResourceManager;
  }
}
