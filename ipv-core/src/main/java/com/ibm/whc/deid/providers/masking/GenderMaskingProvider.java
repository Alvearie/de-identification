/*
 * (C) Copyright IBM Corp. 2016,2021
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

  private static final long serialVersionUID = -5037078935103051994L;

  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  protected transient volatile GenderManager genderResourceManager = null;

  public GenderMaskingProvider(GenderMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty);
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    GenderManager genderManager = getGenderManager();

    Sex sex = genderManager.getValue(identifier);
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

  protected GenderManager getGenderManager() {
    if (genderResourceManager == null) {
      genderResourceManager = (GenderManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.GENDER, null, localizationProperty);
    }
    return genderResourceManager;
  }
}
