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

  protected transient volatile GenderManager genderResourceManager = null;

  public GenderMaskingProvider(GenderMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty);
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    Sex sex = getGenderManager().getRandomValue();
    return sex == null ? null : sex.getName();
  }

  protected GenderManager getGenderManager() {
    if (genderResourceManager == null) {
      genderResourceManager = (GenderManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.GENDER, null, localizationProperty);
    }
    return genderResourceManager;
  }
}
