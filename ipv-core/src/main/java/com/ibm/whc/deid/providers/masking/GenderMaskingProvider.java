/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.models.Sex;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.GenderMaskingProviderConfig;
import com.ibm.whc.deid.util.GenderManager;
import com.ibm.whc.deid.util.ManagerFactory;


public class GenderMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -5037078935103051994L;

  protected GenderManager genderManager;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  protected volatile boolean initialized = false;

  public GenderMaskingProvider(GenderMaskingProviderConfig configuration, String tenantId) {
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

    Sex sex = genderManager.getKey(identifier);
    if (sex == null) {
      debugFaultyInput("sex");
      if (unspecifiedValueHandling == 2) {
        return genderManager.getRandomKey();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    return genderManager.getRandomKey(sex.getNameCountryCode());
  }

  protected void initialize() {
    if (!initialized) {
      genderManager =
          (GenderManager) ManagerFactory.getInstance().getManager(null,
          Resource.GENDER, null, localizationProperty);

      initialized = true;
    }
  }
}
